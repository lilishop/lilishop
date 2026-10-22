package cn.lili.modules.goods.util;

import cn.hutool.core.date.DateUtil;
import cn.hutool.core.util.RandomUtil;

import java.util.Date;

/**
 * 商品编码生成器：前缀 + 日期时间 + 随机后缀
 * SPU编码以 P 开头，SKU编码以 S 开头
 *
 * @author lifenlong
 */
public class GoodsCodeGenerator {

    /**
     * 生成SPU编码，如 P2026061914302542
     */
    public static String spuCode() {
        return "P" + DateUtil.format(new Date(), "yyyyMMddHHmmss") + RandomUtil.randomNumbers(2);
    }

    /**
     * 生成SKU编码，如 S202606191430254201
     */
    public static String skuCode() {
        return "S" + DateUtil.format(new Date(), "yyyyMMddHHmmss") + RandomUtil.randomNumbers(4);
    }
}
