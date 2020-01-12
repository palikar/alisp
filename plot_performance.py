import matplotlib
import matplotlib.pyplot as plt
import os
import sys

plt.style.use('ggplot')
plt.rcParams.update({'figure.max_open_warning': 0})

def remove_prefix(text, prefix):
    if text.startswith(prefix):
        return text[len(prefix):]
    return text


def read_list(file_name):

    with open(file_name, 'r') as list_file:
        line = list_file.readline()

    line = line.strip()
    if line.endswith('\n'): line = line[0:-2]
    return [float(i) for i in line.split(',') if i ]
    
    

def main():


    root_dir = sys.argv[1]
    build_dirs = [b for b in os.listdir(root_dir) if os.path.isdir(os.path.join(root_dir, b)) and "time_build" in b]


    data = {}
    labels = []
    box_data = []
    
    for build_dir in build_dirs:
        path = os.path.join(root_dir, build_dir)
        build_id = remove_prefix(build_dir, "time_build_")
        times = read_list(os.path.join(path, "timing_checks.txt"))
        data[build_id] = times
        labels.append(build_id)
        box_data.append(times)
        

    matplotlib.rc('xtick', labelsize=13) 
    matplotlib.rc('ytick', labelsize=17)

    plt.figure(figsize=(17, 10))

    x = [i for i in range(1, len(data.keys())+1)]
    
    plt.boxplot(box_data, labels=labels)

    for (build, times), i  in zip(data.items(), x):
        plt.scatter([i]*len(times), times, alpha=0.5, linewidths=None)

    
    plt.gca().spines['top'].set_visible(False)
    plt.gca().spines['right'].set_visible(False)

    plt.xlabel("Build")
    plt.ylabel('Performance checks runtime (s)')
    plt.title('Performance comparison', fontsize=17)
    
    plt.savefig(os.path.join(root_dir, 'perfrmance_plot.png'), bbox_inches='tight')


if __name__ == '__main__':
    main()
