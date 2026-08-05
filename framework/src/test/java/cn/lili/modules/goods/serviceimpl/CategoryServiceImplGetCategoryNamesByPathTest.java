package cn.lili.modules.goods.serviceimpl;

import cn.lili.modules.goods.entity.dos.Category;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;

/**
 * categoryPath → 按 level 排序的分类名称列表。
 */
class CategoryServiceImplGetCategoryNamesByPathTest {

    private CategoryServiceImpl categoryService;

    @BeforeAll
    static void initEntityCache() {
        MapperBuilderAssistant assistant = new MapperBuilderAssistant(new MybatisConfiguration(), "");
        TableInfoHelper.initTableInfo(assistant, Category.class);
    }

    @BeforeEach
    void setUp() {
        categoryService = spy(new CategoryServiceImpl());
    }

    @Test
    void returnsEmptyWhenPathBlank() {
        assertThat(categoryService.getCategoryNamesByPath(null)).isEmpty();
        assertThat(categoryService.getCategoryNamesByPath("")).isEmpty();
    }

    @Test
    void returnsNamesOrderedByLevel() {
        Category level1 = new Category();
        level1.setId("c1");
        level1.setName("食品");
        level1.setLevel(1);
        Category level2 = new Category();
        level2.setId("c2");
        level2.setName("巧克力");
        level2.setLevel(2);
        doReturn(List.of(level1, level2)).when(categoryService).listByIdsOrderByLevel(anyList());

        List<String> names = categoryService.getCategoryNamesByPath("c2,c1");

        assertThat(names).containsExactly("食品", "巧克力");
    }
}
