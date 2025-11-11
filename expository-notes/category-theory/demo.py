# demo.py: 范畴学基础概念的Python模拟实现
from typing import Generic, TypeVar, Callable, List

# 类型变量用于模拟范畴中的对象类型
A = TypeVar('A')
B = TypeVar('B')
C = TypeVar('C')


class Morphism(Generic[A, B]):
    """模拟范畴中的态射（morphism）"""
    def __init__(self, dom: A, codom: B, func: Callable[[A], B]):
        self.dom = dom  # 定义域对象
        self.codom = codom  # 陪域对象
        self.func = func  # 态射对应的函数实现

    def __call__(self, x: A) -> B:
        return self.func(x)

    def __matmul__(self, other: 'Morphism[C, A]') -> 'Morphism[C, B]':
        """用 @ 运算符模拟态射复合 (g @ f 表示 g ∘ f)"""
        if other.codom != self.dom:
            raise ValueError("态射复合定义域不匹配")
        return Morphism(other.dom, self.codom, lambda x: self.func(other.func(x)))


class Category(Generic[A]):
    """模拟一个范畴（category）"""
    def __init__(self, objects: List[A], morphisms: List[Morphism]):
        self.objects = objects
        self.morphisms = morphisms

    def identity(self, obj: A) -> Morphism[A, A]:
        """获取对象上的单位态射"""
        return Morphism(obj, obj, lambda x: x)


class Functor(Generic[A, B]):
    """模拟函子（functor）：从范畴C到范畴D的映射"""
    def __init__(self, 
                 source: Category[A], 
                 target: Category[B],
                 map_obj: Callable[[A], B],
                 map_morph: Callable[[Morphism[A, A]], Morphism[B, B]]):
        self.source = source  # 源范畴
        self.target = target  # 目标范畴
        self.map_obj = map_obj  # 对象映射
        self.map_morph = map_morph  # 态射映射

    def __call__(self, obj: A) -> B:
        """函子作用于对象"""
        return self.map_obj(obj)

    def apply_morph(self, morph: Morphism[A, A]) -> Morphism[B, B]:
        """函子作用于态射"""
        return self.map_morph(morph)


# 示例：集合范畴（Set）的模拟
if __name__ == "__main__":
    # 1. 定义集合范畴中的对象（集合）
    set_objects = [int, str, list]  # 以Python类型模拟集合对象

    # 2. 定义态射（函数）
    # 态射f: int → str（整数转字符串）
    f = Morphism(int, str, lambda x: str(x))
    # 态射g: str → list（字符串转字符列表）
    g = Morphism(str, list, lambda s: list(s))
    # 复合态射g ∘ f: int → list
    h = g @ f

    # 3. 构建集合范畴
    set_category = Category(set_objects, [f, g, h])

    # 4. 定义列表函子（List Functor）：Set → Set
    # 对象映射：将集合A映射到列表集合List[A]
    def list_map_obj(a: type) -> type:
        return List[a]  # 用Python的List类型模拟

    # 态射映射：将函数f: A→B映射到f*: List[A]→List[B]（逐元素应用f）
    def list_map_morph(morph: Morphism[A, B]) -> Morphism[List[A], List[B]]:
        return Morphism(
            List[morph.dom], 
            List[morph.codom],
            lambda lst: [morph(x) for x in lst]
        )

    # 实例化列表函子
    list_functor = Functor(
        source=set_category,
        target=set_category,
        map_obj=list_map_obj,
        map_morph=list_map_morph
    )

    # 测试：函子作用于对象
    print("列表函子作用于int对象：", list_functor(int))  # 输出 List[int]

    # 测试：函子作用于态射f（int→str）
    mapped_f = list_functor.apply_morph(f)
    test_list = [1, 2, 3]
    print("函子作用于态射后的结果：", mapped_f(test_list))  # 输出 ['1', '2', '3']