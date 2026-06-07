package cn.lili.modules.finance.export;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.function.Function;

/**
 * Excel 列定义，用于声明表头与行数据取值逻辑。
 *
 * @param <T> 行数据类型
 */
@Getter
@AllArgsConstructor
public class SheetColumn<T> {

    /** 列标题（表头） */
    private final String header;

    /** 从行对象提取单元格值的函数 */
    private final Function<T, Object> valueGetter;
}
