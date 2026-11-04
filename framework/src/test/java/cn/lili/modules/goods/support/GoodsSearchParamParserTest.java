package cn.lili.modules.goods.support;

import cn.lili.modules.goods.entity.dto.GoodsParamsItemDTO;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 双形态解析器：parseAll 保真，parseIndexed 过滤/trim/去重/长度守卫。
 *
 * @author Mike
 * @date 2026-09-03
 */
class GoodsSearchParamParserTest {

    private static final String NESTED = """
            [{"goodsParamsItemDTOList":[{"paramName":"口味","isIndex":1,"required":0,"paramId":"p1","paramValue":"原味","sort":1}],"groupId":"g1","groupName":"食品"}]
            """;
    private static final String NESTED_NO_GROUP_META = """
            [{"goodsParamsItemDTOList":[{"paramName":"内存","isIndex":1,"paramValue":"8G","sort":2}]}]
            """;
    private static final String FLAT = """
            [{"paramName":"颜色","paramValue":"黑色","isIndex":1,"paramId":"c1","sort":3}]
            """;

    @Nested
    class ParseIndexed {

        /**
         * 验证标准分组嵌套结构的商品参数可以解析。
         */
        @Test
        void parsesStandardNested() {
            List<GoodsParamsItemDTO> items = GoodsSearchParamParser.parseIndexed(NESTED);
            assertThat(items).hasSize(1);
            assertThat(items.get(0).getParamName()).isEqualTo("口味");
            assertThat(items.get(0).getParamValue()).isEqualTo("原味");
            assertThat(items.get(0).getIsIndex()).isEqualTo(1);
        }

        /**
         * 验证缺少分组元数据的嵌套参数可以解析。
         */
        @Test
        void parsesNestedWithoutGroupMetadata() {
            List<GoodsParamsItemDTO> items = GoodsSearchParamParser.parseIndexed(NESTED_NO_GROUP_META);
            assertThat(items).hasSize(1);
            assertThat(items.get(0).getParamName()).isEqualTo("内存");
            assertThat(items.get(0).getParamValue()).isEqualTo("8G");
        }

        /**
         * 验证平铺结构的商品参数可以解析。
         */
        @Test
        void parsesFlat() {
            List<GoodsParamsItemDTO> items = GoodsSearchParamParser.parseIndexed(FLAT);
            assertThat(items).hasSize(1);
            assertThat(items.get(0).getParamName()).isEqualTo("颜色");
            assertThat(items.get(0).getParamValue()).isEqualTo("黑色");
        }

        /**
         * 验证混合嵌套及平铺结构的商品参数可以解析。
         */
        @Test
        void parsesMixedNestedAndFlat() {
            String json = """
                    [{"goodsParamsItemDTOList":[{"paramName":"内存","paramValue":"8G","isIndex":1,"sort":1}]},{"paramName":"颜色","paramValue":"黑色","isIndex":1,"sort":2}]
                    """;
            List<GoodsParamsItemDTO> items = GoodsSearchParamParser.parseIndexed(json);
            assertThat(items).extracting(GoodsParamsItemDTO::getParamName).containsExactly("内存", "颜色");
        }

        /**
         * 验证非法参数JSON返回空解析结果。
         */
        @Test
        void illegalJsonReturnsEmpty() {
            assertThat(GoodsSearchParamParser.parseIndexed("{not-json")).isEmpty();
            assertThat(GoodsSearchParamParser.parseIndexed("not an array")).isEmpty();
        }

        /**
         * 验证空值及空参数结构返回空解析结果。
         */
        @Test
        void nullAndEmptyReturnEmpty() {
            assertThat(GoodsSearchParamParser.parseIndexed(null)).isEmpty();
            assertThat(GoodsSearchParamParser.parseIndexed("")).isEmpty();
            assertThat(GoodsSearchParamParser.parseIndexed("[]")).isEmpty();
        }

        /**
         * 验证索引解析排除非索引参数及索引标识为空的参数。
         */
        @Test
        void dropsNonIndexAndNullIndex() {
            String json = """
                    [{"paramName":"A","paramValue":"1","isIndex":0},{"paramName":"B","paramValue":"2"},{"paramName":"C","paramValue":"3","isIndex":1}]
                    """;
            List<GoodsParamsItemDTO> items = GoodsSearchParamParser.parseIndexed(json);
            assertThat(items).extracting(GoodsParamsItemDTO::getParamName).containsExactly("C");
        }

        /**
         * 验证索引解析排除名称或值缺失的参数。
         */
        @Test
        void dropsMissingParamNameOrValue() {
            String json = """
                    [{"paramName":"内存","isIndex":1},{"paramValue":"8G","isIndex":1},{"paramName":"","paramValue":"x","isIndex":1},{"paramName":"y","paramValue":"","isIndex":1}]
                    """;
            assertThat(GoodsSearchParamParser.parseIndexed(json)).isEmpty();
        }

        /**
         * 验证同名同值参数去重并保留最小排序值，空排序按最大值处理。
         */
        @Test
        void dedupsSameNameValueAndKeepsMinSortTreatingNullAsMax() {
            String json = """
                    [{"paramName":"内存","paramValue":"8G","isIndex":1,"paramId":"a","sort":null},{"paramName":"内存","paramValue":"8G","isIndex":1,"paramId":"b","sort":5},{"paramName":"内存","paramValue":"8G","isIndex":1,"paramId":"c","sort":2}]
                    """;
            List<GoodsParamsItemDTO> items = GoodsSearchParamParser.parseIndexed(json);
            assertThat(items).hasSize(1);
            assertThat(items.get(0).getSort()).isEqualTo(2);
        }

        /**
         * 验证索引解析去除参数名称及参数值首尾空白。
         */
        @Test
        void trimsNameAndValue() {
            String json = """
                    [{"paramName":" 内存 ","paramValue":" 8G ","isIndex":1}]
                    """;
            List<GoodsParamsItemDTO> items = GoodsSearchParamParser.parseIndexed(json);
            assertThat(items).hasSize(1);
            assertThat(items.get(0).getParamName()).isEqualTo("内存");
            assertThat(items.get(0).getParamValue()).isEqualTo("8G");
        }

        /**
         * 验证索引解析排除长度超过255的参数字段。
         */
        @Test
        void dropsFieldsLongerThan255() {
            String tooLong = "a".repeat(256);
            String json = """
                    [{"paramName":"%s","paramValue":"ok","isIndex":1},{"paramName":"ok","paramValue":"%s","isIndex":1},{"paramName":"%s","paramValue":"ok","isIndex":1}]
                    """.formatted(tooLong, tooLong, "a".repeat(255));
            List<GoodsParamsItemDTO> items = GoodsSearchParamParser.parseIndexed(json);
            assertThat(items).hasSize(1);
            assertThat(items.get(0).getParamName()).hasSize(255);
        }

        /**
         * 验证索引解析结果均标记为参与索引。
         */
        @Test
        void returnedItemsAlwaysHaveIsIndexOne() {
            String json = """
                    [{"paramName":"内存","paramValue":"8G","isIndex":1},{"paramName":"颜色","paramValue":"黑","isIndex":0}]
                    """;
            List<GoodsParamsItemDTO> items = GoodsSearchParamParser.parseIndexed(json);
            assertThat(items).isNotEmpty();
            assertThat(items).allMatch(item -> Integer.valueOf(1).equals(item.getIsIndex()));
        }
    }

    @Nested
    class ParseAll {

        /**
         * 验证详情参数解析保留不参与索引的参数。
         */
        @Test
        void keepsNonIndexItems() {
            String json = """
                    [{"paramName":"产地","paramValue":"广东","isIndex":0,"paramId":"p0"}]
                    """;
            List<GoodsParamsItemDTO> items = GoodsSearchParamParser.parseAll(json);
            assertThat(items).hasSize(1);
            assertThat(items.get(0).getIsIndex()).isZero();
            assertThat(items.get(0).getParamName()).isEqualTo("产地");
        }

        /**
         * 验证详情参数解析保留长度超过255的字段。
         */
        @Test
        void keepsFieldsLongerThan255() {
            String tooLong = "a".repeat(256);
            String json = """
                    [{"paramName":"%s","paramValue":"%s","isIndex":0}]
                    """.formatted(tooLong, tooLong);
            List<GoodsParamsItemDTO> items = GoodsSearchParamParser.parseAll(json);
            assertThat(items).hasSize(1);
            assertThat(items.get(0).getParamName()).hasSize(256);
            assertThat(items.get(0).getParamValue()).hasSize(256);
        }

        /**
         * 验证详情参数解析保留参数ID不同的同名同值记录。
         */
        @Test
        void keepsDuplicateNameValueWithDifferentParamIds() {
            String json = """
                    [{"paramName":"内存","paramValue":"8G","isIndex":1,"paramId":"a"},{"paramName":"内存","paramValue":"8G","isIndex":0,"paramId":"b"}]
                    """;
            List<GoodsParamsItemDTO> items = GoodsSearchParamParser.parseAll(json);
            assertThat(items).hasSize(2);
            assertThat(items).extracting(GoodsParamsItemDTO::getParamId).containsExactly("a", "b");
        }

        /**
         * 验证详情参数解析保留参数值首尾空白。
         */
        @Test
        void preservesLeadingAndTrailingSpacesOnParamValue() {
            String json = """
                    [{"paramName":" 内存 ","paramValue":" 8G ","isIndex":0}]
                    """;
            List<GoodsParamsItemDTO> items = GoodsSearchParamParser.parseAll(json);
            assertThat(items).hasSize(1);
            assertThat(items.get(0).getParamName()).isEqualTo(" 内存 ");
            assertThat(items.get(0).getParamValue()).isEqualTo(" 8G ");
        }

        /**
         * 验证详情解析同样兼容并规范化嵌套及平铺结构。
         */
        @Test
        void sameFixturesAsIndexedStillNormalizeBothShapes() {
            assertThat(GoodsSearchParamParser.parseAll(NESTED)).extracting(GoodsParamsItemDTO::getParamName).containsExactly("口味");
            assertThat(GoodsSearchParamParser.parseAll(NESTED_NO_GROUP_META)).extracting(GoodsParamsItemDTO::getParamName).containsExactly("内存");
            assertThat(GoodsSearchParamParser.parseAll(FLAT)).extracting(GoodsParamsItemDTO::getParamName).containsExactly("颜色");
            assertThat(GoodsSearchParamParser.parseAll(null)).isEmpty();
            assertThat(GoodsSearchParamParser.parseAll("")).isEmpty();
            assertThat(GoodsSearchParamParser.parseAll("{broken")).isEmpty();
        }
    }
}
