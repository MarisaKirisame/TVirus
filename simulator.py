import pydot
import subprocess

class UF:
    def __init__(self, v):
        self.v = v
        self.parent = None

    def get_root(self):
        if self.parent is None:
            return self
        else:
            self.parent = self.parent.get_root()
            return self.parent

    def value(self):
        return self.get_root().v

    def merge(self, rhs_):
        lhs = self.get_root()
        rhs = rhs_.get_root()
        if lhs != rhs:
            assert lhs.parent is None
            assert rhs.parent is None
            lhs.parent = rhs
            rhs.v += lhs.v

class Node:
    def __init__(self, name):
        self.name = name
        self.cost = None
        self.forward_uf = UF(0)
        self.backward_uf = UF(0)

class Graph:
    def __init__(self):
        self.nodes = set()
        self.alive = set()
        self.edges = {}
        self.backedges = {}
        self.max_cost = 0
        self.last_evicted = None

    def node(self):
        n = Node(str(len(self.nodes)))
        self.nodes.add(n)
        self.alive.add(n)
        self.edges[n] = set()
        self.backedges[n] = set()
        return n

    def edge(self, l, r):
        self.edges[l].add(r)
        self.backedges[r].add(l)

    def cost(self, n):
        assert n is not None
        counted = set()
        value = 1
        assert n.cost is None
        for neighbor in self.edges[n]:
            if neighbor.cost is not None and neighbor.cost.get_root() not in counted:
                counted.add(neighbor.cost.get_root())
                value += neighbor.cost.value()
            if neighbor.backward_uf.get_root() not in counted:
                counted.add(neighbor.backward_uf.get_root())
                value += neighbor.backward_uf.value()
        for neighbor in self.backedges[n]:
            if neighbor.cost is not None and neighbor.cost.get_root() not in counted:
                counted.add(neighbor.cost.get_root())
                value += neighbor.cost.value()
            if neighbor.forward_uf.get_root() not in counted:
                counted.add(neighbor.forward_uf.get_root())
                value += neighbor.forward_uf.value()
        return value

    def evict_node(self, n):
        assert isinstance(n, Node)
        assert n.cost is None
        n.cost = UF(1)
        for neighbor in self.edges[n]:
            if neighbor.cost is not None:
                n.cost.merge(neighbor.cost)
            n.cost.merge(neighbor.backward_uf)
        for neighbor in self.backedges[n]:
            if neighbor.cost is not None:
                n.cost.merge(neighbor.cost)
            n.cost.merge(neighbor.forward_uf)
        self.max_cost = max(self.max_cost, n.cost.value())
        self.alive.remove(n)
        self.last_evicted = n
        return n.cost.value()

    def evict(self):
        assert len(self.alive) > 0
        n = None
        min_cost = None
        for candidate in self.alive:
            assert candidate is not None
            cost = self.cost(candidate)
            if min_cost is None or cost < min_cost:
                n = candidate
                min_cost = cost
        evicted_cost = self.evict_node(n)
        assert min_cost == evicted_cost

    def evict_to(self, limit):
        while len(self.alive) > limit:
            self.evict()

g = Graph()

size = 20

pascal = True

if pascal:
    prev_l = None
    l = [g.node() for _ in range(size)]

    for _ in range(1, size):
        next_l = [g.node() for _ in range(size)]
        for i in range(1, size - 1):
            g.edge(l[i], next_l[i])
            g.edge(l[i-1], next_l[i])
            g.edge(l[i+1], next_l[i])
        # turning it on make very ugly svg and we already can handle this case.
        if False and prev_l is not None:
            for i in range(1, size - 1):
                g.edge(prev_l[i], next_l[i])
                g.edge(prev_l[i-1], next_l[i])
                g.edge(prev_l[i+1], next_l[i])
        prev_l = l
        l = next_l
else:
    l = [g.node() for _ in range(size)]
    for i in range(1, size):
        g.edge(l[i-1], l[i])
    for _ in range(1, size):
        next_l = [g.node() for _ in range(size)]
        for i in range(1, size):
            g.edge(next_l[i-1], next_l[i])
        for i in range(size):
            g.edge(l[i], next_l[i])
        l = next_l

g.evict_to(100)
print(g.max_cost)

edge_to_name = {}
graph = pydot.Dot("my_graph", graph_type="graph")

for n in g.nodes:
    if n in g.alive:
        color = "red"
    elif n == g.last_evicted:
        color = "gold"
    else:
        color = "black"
    if n.cost is None:
        name = n.name
    else:
        name = f"{n.name}({n.cost.value()})"
    edge_to_name[n] = name
    graph.add_node(pydot.Node(name, color=color))

for l in g.edges:
    for r in g.edges[l]:
        graph.add_edge(pydot.Edge(edge_to_name[l], edge_to_name[r]))

graph.write_svg(f"output.svg")
subprocess.run("xdg-open output.svg", shell=True)

