# Markdown 语法介绍

![这是一个图片](https://markdown.land/wp-content/uploads/2021/06/markdown-512px.png)

__Markdown__ 是一种轻量级的标记语言，可以使用简单的标记符号来方便地书写文本并转换为 HTML。它是一种使用纯文本格式的标记语言，可以方便的转换为多种其他格式，如 PDF、HTML、Word 等。
Markdown 是一种轻量级的标记语言，可以使用简单的标记符号来方便地书写文本并转换为 HTML。它是一种使用纯文本格式的标记语言，可以方便的转换为多种其他格式，如 PDF、HTML、Word 等。
Markdown 是一种轻量级的标记语言，可以使用简单的标记符号来方便地书写文本并转换为 HTML。它是一种使用纯文本格式的标记语言，可以方便的转换为多种其他格式，如 PDF、HTML、Word 等。

## 粗体 斜体
**这是一段粗体***这是一段斜体****这是一段斜体+粗体***
__这是一段粗体___这是一段斜体____这是一段斜体+粗体___
**这是一段粗体**_这是一段斜体_***这是一段斜体+粗体***

## 无序列表
- 列表项 1
  嵌入文本 1
  - 嵌入项 1
  - 嵌入项 2
- 列表项 2
- 列表项 3

* [链接1](https://www.example.com)
* [链接2](https://www.example.com)
* [链接3](https://www.example.com)


## 有序列表
1. 有序列表项 1
1. **__有序列表项 2__**
1. 有序列表项 3

## 链接
[这是一个链接](https://www.example.com) <br>
<https://www.example.com> <br>
<user@example.com> <br>
[Example][link] <br>

## 引用块
> 这是一段引用文本  
> 一种使用纯文本格式的标记语言，可以方便的转换为多种其他格式<br>
> [引用链接](https://www.example.com)

## 引用嵌套
> # 一级标题
> 这是一段引用文本  
> 一种使用纯文本格式的标记语言，可以方便的转换为多种其他格式<br>
> [引用链接](https://www.example.com)
> 
> This text is ***really important***.
> Italicized text is the _cat's meow_.
>
> ## 这是一个链接 [Markdown语法](https://markdown.com.cn)。
> - First item
> - Second item
> - Third item
> - Fourth item
>
> > Dorothy followed her through many of the beautiful rooms in her castle.
> >
> > The Witch bade her clean the pots and kettles and sweep the floor and keep the fire fed with wood.
>>> # 一级标题
>>> 这是一段引用文本  
>>> 一种使用纯文本格式的标记语言，可以方便的转换为多种其他格式<br>
>>> [引用链接](https://www.example.com)
>>> 
>>> This text is ***really important***.
>>> Italicized text is the _cat's meow_.
>>>
>>> ## 这是一个链接 [Markdown语法](https://markdown.com.cn)。
>>> - First item
>>> - Second item
>>> - Third item
>>> - Fourth item
>>>
>>> >  Dorothy followed her through many of the beautiful rooms in her castle.
>>> >
>>>>  The Witch bade her clean the pots and kettles and sweep the floor and keep the fire fed with wood.


## 代码块
```
fn main() {
    println!("Hello, world!");
}
```

## 行内代码
This is an inline code block: `print("Hello, World!")`

## 分隔线

---

## 转义字符

\* 列表1
\* 列表2

This is an inline code block: \`print("Hello, World!")`


[link]: https://www.example.com "example"