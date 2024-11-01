"""Plots cross-benchmark evaluation data"""

import matplotlib.pyplot as plt
from matplotlib.ticker import FormatStrFormatter
from scipy.stats import spearmanr

from typing import List, Tuple
from argparse import ArgumentParser
from pathlib import Path
import json
import math

from platforms.fpcore import FPCore
from platforms.shim import shim_pareto

# Globals
invert_axes = True # (speedup, accuracy) vs. (cost, error)
use_time = True # time vs cost

input_color = 'black'
platform_color = '#0072B2'
supported_color = '#009E73'
desugared_color = '#D55E00'

dot_size = 4
dot_size2 = 3

input_style = 's'
platform_style = '.'
supported_style = '2'
desugared_style = '_'

# preferred order
order = [
    'arith', 'arith-fma', 'avx',
    'c', 'julia', 'python',
    'vdt', 'fdlibm', 'numpy'
]

# preferred name
display_names = {
    'arith': 'Arith',
    'arith-fma': 'Arith+FMA',
    'avx': 'AVX',
    'c': 'C',
    'julia': 'Julia',
    'python': 'Python',
    'vdt': 'vdt',
    'fdlibm': 'fdlibm',
    'numpy': 'NumPy'
}

# plot extensions
plt_exts = ['png', 'pdf']

######################################
# Utils

def core_max_error(core: FPCore) -> int:
    if core.prec == 'binary64':
        return 64
    elif core.prec == 'binary32':
        return 32
    else:
        raise RuntimeError('Unknown precision', core.prec)
    
def flip_point(input_cost: float, max_error: float, pt: Tuple[float, float]):
    """Transforms `(cost, error)` points into `(speedup, accuracy)` points."""
    cost, error = pt
    return (input_cost / cost, max_error - error)

#######################################
# Cost vs. Time

def platform_cost_time(info):
    costs = []
    times = []
    for input_info in info['cores']:
        for core_info in input_info['platform_cores']:
            core = FPCore.from_json(core_info['platform_core'])
            costs.append(core.cost)
            times.append(core.time)
    return costs, times

def plot_time_all(output_dir: Path, entries):
    print(f'Plotting time for all platforms')
    size = 8

    names = []
    for name, _ in entries:
        names.append(name)

    names = sorted(names)
    num_platforms = len(names)
    nrows = (num_platforms + 2) // 3 # ceil_div(num_platforms, 3)
    fig, axs = plt.subplots(ncols=3, nrows=nrows, figsize=((size, size)))

    time_unit = None
    for _, info in entries:
        if time_unit is None:
            time_unit = info['time_unit']
        elif time_unit != info['time_unit']:
            raise RuntimeError('Time units do not match')

    fig.supxlabel("Estimated cost")
    fig.supylabel(f"Run time ({time_unit})")

    min_rho = math.inf
    max_rho = -math.inf
    for i, (name, info) in enumerate(entries):
        costs, times = platform_cost_time(info)
        ax = axs[i // 3, i % 3] if num_platforms > 3 else axs[i]
        ax.scatter(costs, times, color=platform_color)
        ax.set_title(display_names[name])

        rho = spearmanr(costs, times).statistic
        if rho < min_rho:
            min_rho = rho
        if rho > max_rho:
            max_rho = rho
   
    for i in range(len(names), 3 * nrows):
        ax = axs[i // 3, i % 3] if num_platforms > 3 else axs[i]
        fig.delaxes(ax)

    plt.tight_layout()
    for ext in plt_exts:
        plt.savefig(output_dir.joinpath(f'cost-vs-time.{ext}'), dpi=300)
    plt.close()

    print('min_rho:', min_rho)
    print('max_rho:', max_rho)

#######################################
# Baseline

def comparison_frontiers(info):
    num_input = 0
    num_platform = 0
    num_supported = 0
    num_desugared = 0

    input_cores: List[FPCore] = []
    platform_cores: List[FPCore] = []
    supported_cores: List[FPCore] = []
    desugared_cores: List[FPCore] = []
    for core_info in info['cores']:
        input_core = FPCore.from_json(core_info['input_core'])
        platform = list(map(FPCore.from_json, core_info['platform_cores']))
        supported = list(map(FPCore.from_json, core_info['supported_cores']))
        desugared = list(map(FPCore.from_json, core_info['desugared_cores']))

        num_input += 1
        if platform:
            num_platform += 1
        elif supported:
            num_supported += 1
        elif desugared:
            num_desugared += 1

        if platform and supported and desugared:
            input_cores.append(input_core)
            platform_cores += platform
            supported_cores += supported
            desugared_cores += desugared

    # compute starting point
    max_error = sum(map(lambda c: core_max_error(c), input_cores))
    input_costs, input_errs = zip(*map(lambda c: (c.time if use_time else c.cost, c.err), input_cores))
    input_cost, input_error = sum(input_costs), sum(input_errs)
    flip = lambda pt: flip_point(input_cost, max_error, pt)

    # compute (cost, error) frontiers
    platform_frontier, supported_frontier, desugared_frontier = \
        shim_pareto(platform_cores, supported_cores, desugared_cores, use_time=use_time)

    # compute (speedup, accuracy) frontiers
    input_speedup, input_accuracy = flip((input_cost, input_error))
    platform_frontier2 = list(map(flip, platform_frontier))
    supported_frontier2 = list(map(flip, supported_frontier))
    desugared_frontier2 = list(map(flip, desugared_frontier))

    return (input_cost, input_error), (input_speedup, input_accuracy), num_input, \
        platform_frontier, platform_frontier2, num_platform, \
        supported_frontier, supported_frontier2, num_supported, \
        desugared_frontier, desugared_frontier2, num_desugared

# ignore = ['Toniolo and Linder, Equation (7)', 'Toniolo and Linder, Equation (10-)', 'sqrt A (should all be same)', 'sqrt B (should all be same)', 'sqrt C (should all be same)', 'sqrt D (should all be same)', 'sqrt E (should all be same)', 'bug366, discussion (missed optimization)', 'x / (x^2 + 1)', 'Complex division, imag part', 'Kahan p9 Example', 'cos2 (problem 3.4.1)', 'Linear.Quaternion:$clog from linear-1.19.1.3', 'Diagrams.TwoD.Apollonian:descartes from diagrams-contrib-1.3.0.5', 'Diagrams.TwoD.Apollonian:initialConfig from diagrams-contrib-1.3.0.5, B', 'AI.Clustering.Hierarchical.Internal:ward from clustering-0.2.1', 'Statistics.Math.RootFinding:ridders from math-functions-0.1.5.2', 'Data.Array.Repa.Algorithms.Pixel:doubleRmsOfRGB8 from repa-algorithms-3.4.0.1', 'Jmat.Real.dawson'] + ['expfmod (used to be hard to sample)', '2-ancestry mixing, positive discriminant', 'Simplification of discriminant from scale-rotated-ellipse', 'Distance on a great circle', 'Jmat.Real.erf']
ignore = []

def comparison_frontiers2(info):
    input_cores: List[FPCore] = []
    platform_cores: List[FPCore] = []
    supported_cores: List[FPCore] = []
    desugared_cores: List[FPCore] = []
    for core_info in info['cores']:
        input_core = FPCore.from_json(core_info['input_core'])
        platform = list(map(FPCore.from_json, core_info['platform_cores']))
        supported = list(map(FPCore.from_json, core_info['supported_cores']))
        desugared = list(map(FPCore.from_json, core_info['desugared_cores']))

        if input_core.name in ignore:
            continue

        if platform and supported and desugared:
            input_cores.append(input_core)
            platform_cores += platform
            supported_cores += supported
            desugared_cores += desugared

    # compute starting point
    max_error = sum(map(lambda c: core_max_error(c), input_cores))
    input_times, input_errs = zip(*map(lambda c: (c.time, c.err), input_cores))
    input_time, input_acc = sum(input_times), max_error - sum(input_errs)

    # compute (time, error) frontiers
    platform_frontier, supported_frontier, desugared_frontier = \
        shim_pareto(platform_cores, supported_cores, desugared_cores, use_time=use_time)

    # compute (time, accuracy) frontiers
    platform_frontier = list(map(lambda pt: (pt[0], max_error - pt[1]), platform_frontier))
    supported_frontier = list(map(lambda pt: (pt[0], max_error - pt[1]), supported_frontier))
    desugared_frontier = list(map(lambda pt: (pt[0], max_error - pt[1]), desugared_frontier))
    return (input_time, input_acc), platform_frontier, supported_frontier, desugared_frontier

def has_platform_cores(info):
    input_cores: List[FPCore] = []
    platform_cores: List[FPCore] = []
    supported_cores: List[FPCore] = []
    desugared_cores: List[FPCore] = []
    for core_info in info['cores']:
        input_core = FPCore.from_json(core_info['input_core'])
        platform = list(map(FPCore.from_json, core_info['platform_cores']))
        supported = list(map(FPCore.from_json, core_info['supported_cores']))
        desugared = list(map(FPCore.from_json, core_info['desugared_cores']))

        if platform and supported and desugared:
            input_cores.append(input_core)
            platform_cores += platform
            supported_cores += supported
            desugared_cores += desugared

    return len(input_cores) > 0


def normalize(pts: List[Tuple[float, float]], pts2: List[Tuple[float, float]]):
    """Normalizes each `y` value of `pts` in terms of multiples of the `y` value of `pts`."""
    # super naive algorithm follows
    norm_pts: list[tuple[float, float]] = []
    for pt in pts:
        before = max(filter(lambda pt2: pt2[0] <= pt[0], pts2), key=lambda pt: pt[0], default=None)
        after = min(filter(lambda pt2: pt2[0] >= pt[0], pts2), key=lambda pt: pt[0], default=None)
        match (before, after):
            case (None, None):
                # what happened?
                raise NotImplementedError
            case (before, None):
                # we discard this point to bias towards the baseline
                pass
            case (None, after):
                # at or below minimum x => apply linear interpolation using last points of `pts2`
                if len(pts2) > 1:
                    p1, p2 = pts2[0], pts2[1]
                    m = (p2[1] - p1[1]) / (p2[0] - p1[0])
                    y = m * (pt[0] - p1[0]) + p1[1]
                    p = (pt[0], pt[1] / y)
                    norm_pts.append(p)
                else:
                    p1 = pts2[0]
                    y = p1[0]
                    p = (pt[0], pt[1] / y)
                    norm_pts.append(p)
            case (before, after):
                # in-between two points => apply linear interpolation
                if before[0] == after[0]:
                    # bail if the points are the same, take the min
                    y = min(before[1], after[1])
                    p = (pt[0], pt[1] / y)
                    norm_pts.append(p)
                else:
                    t = (pt[0] - before[0]) / (after[0] - before[0])
                    y = after[1] * t + (1 - t) * before[1]
                    p = (pt[0], pt[1] / y)
                    norm_pts.append(p)

    return norm_pts


def plot_baseline_all(output_dir: Path, entries):
    """Entire baseline comparison (N)."""
    print(f'Plotting all baseline comparison under {output_dir}')
    size = 8

    names = []
    for name, _ in entries:
        names.append(name)

    names = sorted(names)
    num_platforms = len(names)
    nrows = (num_platforms + 2) // 3 # ceil_div(num_platforms, 3)
    fig, axs = plt.subplots(ncols=3, nrows=nrows, figsize=((size, size)))

    assert invert_axes and use_time

    # first mode
    fig, axs = plt.subplots(ncols=3, nrows=nrows, figsize=((size, size)))
    fig.supxlabel('Sum of accuracy log2(ULP)')
    fig.supylabel('Speedup')

    for i, (name, info) in enumerate(entries):
        ax = axs[i // 3, i % 3] if num_platforms > 3 else axs[i]
        ax.set_title(display_names[name], size='medium')

        if has_platform_cores(info):
            input_pt, platform_frontier, supported_frontier, desugared_frontier = comparison_frontiers2(info)

            # flip frontiers (x, y) -> (y, x)
            input_pt = (input_pt[1], input_pt[0])
            platform_frontier = list(map(lambda pt: (pt[1], pt[0]), platform_frontier))
            supported_frontier = list(map(lambda pt: (pt[1], pt[0]), supported_frontier))
            desugared_frontier = list(map(lambda pt: (pt[1], pt[0]), desugared_frontier))

            # normalize over `input_pt[1]`
            platform_frontier = list(map(lambda pt: (pt[0], input_pt[1] / pt[1]), platform_frontier))
            desugared_frontier = list(map(lambda pt: (pt[0], input_pt[1] / pt[1]), desugared_frontier))
            supported_frontier = list(map(lambda pt: (pt[0], input_pt[1] / pt[1]), supported_frontier))
            input_pt = (input_pt[0], 1.0)

            # sort frontiers by y
            platform_frontier.sort(key=lambda pt: pt[0])
            supported_frontier.sort(key=lambda pt: pt[0])
            desugared_frontier.sort(key=lambda pt: pt[0])

            # platform_max = max(map(lambda pt: pt[0], platform_frontier))
            # desugared_max = max(map(lambda pt: pt[0], desugared_frontier))

            # relative_frontier = desugared_frontier
            # input_pt = normalize([input_pt], relative_frontier)[0]
            # platform_frontier = normalize(platform_frontier, relative_frontier)
            # supported_frontier = normalize(supported_frontier, relative_frontier)
            # desugared_frontier = normalize(desugared_frontier, relative_frontier)

            # decompose frontiers
            input_x, input_y = input_pt
            platform_xs, platform_ys = zip(*platform_frontier)
            supported_xs, supported_ys = zip(*supported_frontier)
            desugared_xs, desugared_ys = zip(*desugared_frontier)

            # plot
            ax.plot([input_x], [input_y], input_style, color=input_color, )
            ax.plot(platform_xs, platform_ys, platform_style, color=platform_color, markersize=dot_size2)
            ax.plot(supported_xs, supported_ys, supported_style, color=supported_color, mfc='none', markersize=dot_size2)
            ax.plot(desugared_xs, desugared_ys, desugared_style, color=desugared_color, markersize=dot_size2)

            # y-axis formatting
            ax.yaxis.set_major_formatter(FormatStrFormatter('%.1f'))
            ymax = max(map(lambda pt: pt[1], platform_frontier))
            print(name, ymax)

            if name == 'avx' or name == 'arith' or name == 'arith-fma':
                ax.set(ylim=(0, 3.0))
            elif name == 'c' or name == 'vdt' or name == 'fdlibm':
                ax.set(ylim=(0, 10.0))
            elif name == 'numpy':
                ax.set(ylim=(0, 15.0))
            elif name == 'python':
                ax.set(ylim=(0, 2.0))
            elif name == 'julia':
                ax.set(ylim=(0, 1.5))
            else:
                raise NotImplementedError

    for i in range(len(names), 3 * nrows):
        ax = axs[i // 3, i % 3] if num_platforms > 3 else axs[i]
        fig.delaxes(ax)

    plt.tight_layout()
    for ext in plt_exts:
        plt.savefig(output_dir.joinpath(f'baseline-pareto.{ext}'), dpi=300)
    plt.close()

    # second mode
    fig, axs = plt.subplots(ncols=3, nrows=nrows, figsize=((size, size)))
    fig.supxlabel('Sum of accuracy log2(ULP)')
    fig.supylabel('Speedup')

    for i, (name, info) in enumerate(entries):
        ax = axs[i // 3, i % 3] if num_platforms > 3 else axs[i]
        ax.set_title(display_names[name], size='medium')

        if has_platform_cores(info):
            input_pt, platform_frontier, supported_frontier, desugared_frontier = comparison_frontiers2(info)
            
            # flip frontiers (x, y) -> (y, x)
            input_pt = (input_pt[1], input_pt[0])
            platform_frontier = list(map(lambda pt: (pt[1], pt[0]), platform_frontier))
            supported_frontier = list(map(lambda pt: (pt[1], pt[0]), supported_frontier))
            desugared_frontier = list(map(lambda pt: (pt[1], pt[0]), desugared_frontier))

            # normalize over `input_pt[1]`
            platform_frontier = list(map(lambda pt: (pt[0], input_pt[1] / pt[1]), platform_frontier))
            desugared_frontier = list(map(lambda pt: (pt[0], input_pt[1] / pt[1]), desugared_frontier))
            supported_frontier = list(map(lambda pt: (pt[0], input_pt[1] / pt[1]), supported_frontier))
            input_pt = (input_pt[0], 1.0)
            
            # sort frontiers by y
            platform_frontier.sort(key=lambda pt: pt[0])
            supported_frontier.sort(key=lambda pt: pt[0])
            desugared_frontier.sort(key=lambda pt: pt[0])

            # platform_max = max(map(lambda pt: pt[0], platform_frontier))
            # desugared_max = max(map(lambda pt: pt[0], desugared_frontier))

            relative_frontier = desugared_frontier
            input_pt = normalize([input_pt], relative_frontier)[0]
            platform_frontier = normalize(platform_frontier, relative_frontier)
            supported_frontier = normalize(supported_frontier, relative_frontier)
            desugared_frontier = normalize(desugared_frontier, relative_frontier)

            # decompose frontiers
            input_x, input_y = input_pt
            platform_xs, platform_ys = zip(*platform_frontier)
            supported_xs, supported_ys = zip(*supported_frontier)
            desugared_xs, desugared_ys = zip(*desugared_frontier)

            # plot
            ax.plot([input_x], [input_y], input_style, color=input_color)
            ax.plot(platform_xs, platform_ys, platform_style, color=platform_color, markersize=dot_size)
            ax.plot(supported_xs, supported_ys, supported_style, color=supported_color, mfc='none', markersize=dot_size)
            ax.plot(desugared_xs, desugared_ys, desugared_style, color=desugared_color, markersize=dot_size)

            # y-axis formatting
            ax.yaxis.set_major_formatter(FormatStrFormatter('%.1f'))
            ymax = max(map(lambda pt: pt[1], platform_frontier))
            print(name, ymax)
            if ymax > 2:
                ax.set(ylim=(0, 4))
            elif ymax > 1.25:
                ax.set(ylim=(0, 2))
            else:
                ax.set(ylim=(0, 1.5))

    for i in range(len(names), 3 * nrows):
        ax = axs[i // 3, i % 3] if num_platforms > 3 else axs[i]
        fig.delaxes(ax)
    
    plt.tight_layout()
    for ext in plt_exts:
        plt.savefig(output_dir.joinpath(f'baseline-pareto2.{ext}'), dpi=300)
    plt.close()

def plot_c_pareto(output_dir: Path, info):
    """Plots Chassis vs. Clang across all benchmarks"""
    print('Plotting Chassis vs. Clang (C)')
    size = 4.5
    plt.figure(figsize=(size, size))

    assert invert_axes and use_time

    input_cores: List[FPCore] = []
    platform_cores: List[FPCore] = []
    for core_info in info['cores']:
        core_infos = core_info['platform_cores']
        if len(core_infos) > 0:
            input_cores.append(FPCore.from_json(core_info['input_core']))
            for platform_core_info in core_infos:
                platform_cores.append(FPCore.from_json(platform_core_info['platform_core']))
    
    # compute starting point
    max_error = sum(map(lambda c: core_max_error(c), input_cores))

    o0_time = None
    for flags, times, errors in info['extra']:
        if '-O0' in flags and '-ffast-math' not in flags:
            o0_time = sum(times)
            break

    if o0_time is None:
        raise RuntimeError('Could not find baseline configuration: -O0')
    flip = lambda pt: flip_point(o0_time, max_error, pt)

    exacts = []
    fasts = []
    for flags, times, errors in info['extra']:
        if times == []:
            continue

        input_time, input_error = sum(times), sum(errors)
        input_speedup, input_accuracy = flip((input_time, input_error))
        input_x = input_speedup if invert_axes else input_time
        input_y = input_accuracy if invert_axes else input_error
        if '-ffast-math' in flags:
            fasts.append((input_x, input_y))
        else:
            exacts.append((input_x, input_y))

    exact_xs, exact_ys = zip(*exacts)
    plt.plot(exact_xs, exact_ys, 'X', color=input_color, label='Clang')

    fast_xs, fast_ys = zip(*fasts)
    plt.plot(fast_xs, fast_ys, 'P', color=desugared_color, label='Clang (fast-math)')

    # compute (speedup, accuracy) frontiers
    frontier, *_ = shim_pareto(platform_cores, use_time=use_time)
    frontier2 = list(map(flip, frontier))

    xlabel = f'Speedup' if use_time else 'Estimated speedup'
    ylabel = 'Sum of accuracy log2(ULP)'
    xs, ys = zip(*frontier2)

    plt.plot(xs, ys, platform_style, color=platform_color, label='Chassis', markersize=dot_size)
    plt.xlabel(xlabel, fontsize=12)
    plt.ylabel(ylabel, fontsize=12)
    plt.xticks(fontsize=10)
    plt.yticks(fontsize=10)
    plt.tight_layout()

    for ext in plt_exts:
        plt.savefig(output_dir.joinpath(f'c-pareto.{ext}'), dpi=300)
    plt.close()


#######################################
# Entrypoint

def plot_subsuite(output_dir: Path, report):
    baseline_by_platform = dict()
    for name, platform_info in report.items():
        platform_info = report[name]
        for field, field_info in platform_info.items():
            if field == 'compare':
                for name2, compare_info in field_info.items():
                    if name2 == 'baseline':
                        for core_info in compare_info['cores']:
                            input_core = core_info['input_core']
                            supported_cores = core_info['supported_cores']
                            desugared_cores = core_info['desugared_cores']

                            for core in supported_cores:
                                if core['time'] is None or core['err'] is None:
                                    print(f'SUPPORTED: missing data {core['name']}: {core['time']}')
                                    supported_cores.remove(core)

                            for core in desugared_cores:
                                if core['time'] is None:
                                    print(f'DESUGARED: missing data {core['name']}: {core['time']}')
                                    core['time'] = input_core['time']
                                    desugared_cores.remove(core)

                        if name in baseline_by_platform:
                            baseline_by_platform[name]['cores'] += compare_info['cores']
                        else:
                            baseline_by_platform[name] = compare_info

    baseline_reports = []
    for name in sorted(report.keys(), key=lambda k: order.index(k)):
        baseline_reports.append((name, baseline_by_platform[name]))
    plot_baseline_all(output_dir, baseline_reports)


def main():
    parser = ArgumentParser(description='Herbie platforms eval')
    parser.add_argument('output_dir', help='path to evaluation output', type=str)
    args = parser.parse_args()
    output_dir = Path(args.output_dir)

    improve_by_platform = dict()
    baseline_by_platform = dict()
    for bench_dir in output_dir.iterdir():
        if bench_dir.is_dir():
            json_path = bench_dir.joinpath('results.json')
            with open(json_path, 'r') as f:
                report = json.load(f)

            for name, platform_info in report.items():
                platform_info = report[name]
                for field, field_info in platform_info.items():
                    if field == 'improve':
                        if name in improve_by_platform:
                            improve_by_platform[name]['cores'] += field_info['cores']
                            if improve_by_platform[name]['extra'] is None:
                                improve_by_platform[name]['extra'] = field_info['extra']
                            elif field_info['extra'] is not None:
                                for config1, config2 in zip(improve_by_platform[name]['extra'], field_info['extra']):
                                    _, times1, errors1 = config1
                                    _, times2, errors2 = config2
                                    times1 += times2
                                    errors1 += errors2
                        else:
                            improve_by_platform[name] = field_info

                    elif field == 'compare':
                        for name2, compare_info in field_info.items():
                            if name2 == 'baseline':
                                for core_info in compare_info['cores']:
                                    input_core = core_info['input_core']
                                    supported_cores = core_info['supported_cores']
                                    desugared_cores = core_info['desugared_cores']

                                    for core in supported_cores:
                                        if core['time'] is None or core['err'] is None:
                                            print(f'SUPPORTED: missing data {core['name']}: {core['time']}')
                                            supported_cores.remove(core)

                                    for core in desugared_cores:
                                        if core['time'] is None:
                                            print(f'DESUGARED: missing data {core['name']}: {core['time']}')
                                            core['time'] = input_core['time']
                                            desugared_cores.remove(core)

                                if name in baseline_by_platform:
                                    baseline_by_platform[name]['cores'] += compare_info['cores']
                                else:
                                    baseline_by_platform[name] = compare_info

    if 'c' in improve_by_platform:
        plot_c_pareto(output_dir, improve_by_platform['c'])

    improve_reports = []
    baseline_reports = []
    for name in sorted(report.keys(), key=lambda k: order.index(k)):
        improve_reports.append((name, improve_by_platform[name]))
        baseline_reports.append((name, baseline_by_platform[name]))
        
    plot_time_all(output_dir, improve_reports)
    plot_baseline_all(output_dir, baseline_reports)


if __name__ == "__main__":
    main()
