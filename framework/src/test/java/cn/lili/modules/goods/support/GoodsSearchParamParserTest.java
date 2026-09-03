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

        @Test
        void parsesStandardNested() {
            List<GoodsParamsItemDTO> items = GoodsSearchParamParser.parseIndexed(NESTED);
            assertThat(items).hasSize(1);
            assertThat(items.get(0).getParamName()).isEqualTo("口味");
            assertThat(items.get(0).getParamValue()).isEqualTo("原味");
            assertThat(items.get(0).getIsIndex()).isEqualTo(1);
        }

        @Test
        void parsesNestedWithoutGroupMetadata() {
            List<GoodsParamsItemDTO> items = GoodsSearchParamParser.parseIndexed(NESTED_NO_GROUP_META);
            assertThat(items).hasSize(1);
            assertThat(items.get(0).getParamName()).isEqualTo("内存");
            assertThat(items.get(0).getParamValue()).isEqualTo("8G");
        }

        @Test
        void parsesFlat() {
            List<GoodsParamsItemDTO> items = GoodsSearchParamParser.parseIndexed(FLAT);
            assertThat(items).hasSize(1);
            assertThat(items.get(0).getParamName()).isEqualTo("颜色");
            assertThat(items.get(0).getParamValue()).isEqualTo("黑色");
        }

        @Test
        void parsesMixedNestedAndFlat() {
            String json = """
                    [{"goodsParamsItemDTOList":[{"paramName":"内存","paramValue":"8G","isIndex":1,"sort":1}]},{"paramName":"颜色","paramValue":"黑色","isIndex":1,"sort":2}]
                    """;
            List<GoodsParamsItemDTO> items = GoodsSearchParamParser.parseIndexed(json);
            assertThat(items).extracting(GoodsParamsItemDTO::getParamName).containsExactly("内存", "颜色");
        }

        @Test
        void illegalJsonReturnsEmpty() {
            assertThat(GoodsSearchParamParser.parseIndexed("{not-json")).isEmpty();
            assertThat(GoodsSearchParamParser.parseIndexed("not an array")).isEmpty();
        }

        @Test
        void nullAndEmptyReturnEmpty() {
            assertThat(GoodsSearchParamParser.parseIndexed(null)).isEmpty();
            assertThat(GoodsSearchParamParser.parseIndexed("")).isEmpty();
            assertThat(GoodsSearchParamParser.parseIndexed("[]")).isEmpty();
        }

        @Test
        void dropsNonIndexAndNullIndex() {
            String json = """
                    [{"paramName":"A","paramValue":"1","isIndex":0},{"paramName":"B","paramValue":"2"},{"paramName":"C","paramValue":"3","isIndex":1}]
                    """;
            List<GoodsParamsItemDTO> items = GoodsSearchParamParser.parseIndexed(json);
            assertThat(items).extracting(GoodsParamsItemDTO::getParamName).containsExactly("C");
        }

        @Test
        void dropsMissingParamNameOrValue() {
            String json = """
                    [{"paramName":"内存","isIndex":1},{"paramValue":"8G","isIndex":1},{"paramName":"","paramValue":"x","isIndex":1},{"paramName":"y","paramValue":"","isIndex":1}]
                    """;
            assertThat(GoodsSearchParamParser.parseIndexed(json)).isEmpty();
        }

        @Test
        void dedupsSameNameValueAndKeepsMinSortTreatingNullAsMax() {
            String json = """
                    [{"paramName":"内存","paramValue":"8G","isIndex":1,"paramId":"a","sort":null},{"paramName":"内存","paramValue":"8G","isIndex":1,"paramId":"b","sort":5},{"paramName":"内存","paramValue":"8G","isIndex":1,"paramId":"c","sort":2}]
                    """;
            List<GoodsParamsItemDTO> items = GoodsSearchParamParser.parseIndexed(json);
            assertThat(items).hasSize(1);
            assertThat(items.get(0).getSort()).isEqualTo(2);
        }

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

        @Test
        void keepsDuplicateNameValueWithDifferentParamIds() {
            String json = """
                    [{"paramName":"内存","paramValue":"8G","isIndex":1,"paramId":"a"},{"paramName":"内存","paramValue":"8G","isIndex":0,"paramId":"b"}]
                    """;
            List<GoodsParamsItemDTO> items = GoodsSearchParamParser.parseAll(json);
            assertThat(items).hasSize(2);
            assertThat(items).extracting(GoodsParamsItemDTO::getParamId).containsExactly("a", "b");
        }

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
