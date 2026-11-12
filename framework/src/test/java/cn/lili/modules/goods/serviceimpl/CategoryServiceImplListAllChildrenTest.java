package cn.lili.modules.goods.serviceimpl;

import cn.lili.modules.goods.entity.dos.Category;
import cn.lili.modules.goods.entity.vos.CategoryVO;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;

/**
 * 分类树子分类递归查询单元测试
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
class CategoryServiceImplListAllChildrenTest {

    private CategoryServiceImpl categoryService;

    /**
     * 构建分类节点
     *
     * @param id        分类ID
     * @param name      分类名称
     * @param parentId  父分类ID
     * @param children  子分类列表
     * @return 分类VO
     */
    private CategoryVO buildCategory(String id, String name, String parentId, List<CategoryVO> children) {
        CategoryVO vo = new CategoryVO();
        vo.setId(id);
        vo.setName(name);
        vo.setParentId(parentId);
        vo.setSortOrder(BigDecimal.ZERO);
        if (children != null) {
            vo.setChildren(new ArrayList<>(children));
        }
        return vo;
    }

    /**
     * 初始化当前测试使用的MyBatis-Plus实体元数据
     */
    @BeforeAll
    static void initEntityCache() {
        MapperBuilderAssistant assistant = new MapperBuilderAssistant(new MybatisConfiguration(), "");
        TableInfoHelper.initTableInfo(assistant, Category.class);
    }

    /**
     * 初始化当前用例的模拟依赖及测试数据
     */
    @BeforeEach
    void setUp() {
        categoryService = spy(new CategoryServiceImpl());

        // 构造分类树：
        // 顶级1 (top1):
        //   - 子1_1 (sub1_1):
        //       - 叶子1_1_1 (leaf1_1_1)
        //   - 子1_2 (sub1_2):
        //       - 叶子1_2_1 (leaf1_2_1)
        // 顶级2 (top2):
        //   - 子2_1 (sub2_1)
        CategoryVO leaf111 = buildCategory("leaf1_1_1", "叶子1-1-1", "sub1_1", null);
        CategoryVO sub11 = buildCategory("sub1_1", "子分类1-1", "top1", List.of(leaf111));

        CategoryVO leaf121 = buildCategory("leaf1_2_1", "叶子1-2-1", "sub1_2", null);
        CategoryVO sub12 = buildCategory("sub1_2", "子分类1-2", "top1", List.of(leaf121));

        CategoryVO top1 = buildCategory("top1", "顶级1", "0", List.of(sub11, sub12));

        CategoryVO sub21 = buildCategory("sub2_1", "子分类2-1", "top2", null);
        CategoryVO top2 = buildCategory("top2", "顶级2", "0", List.of(sub21));

        doReturn(List.of(top1, top2)).when(categoryService).categoryTree();
    }

    /**
     * 验证 parentId 为 "0" 时返回完整分类树
     */
    @Test
    @DisplayName("parentId为0时应返回完整分类树")
    void listAllChildren_whenParentIdZero_returnsFullTree() {
        List<CategoryVO> result = categoryService.listAllChildren("0");
        assertThat(result).hasSize(2);
        assertThat(result.stream().map(CategoryVO::getId)).containsExactly("top1", "top2");
    }

    /**
     * 验证查询第一个顶级分类时正常返回其子分类
     */
    @Test
    @DisplayName("查询第一个顶级分类时应返回其子分类列表")
    void listAllChildren_whenParentIdIsFirstTopCategory_returnsFirstChildren() {
        List<CategoryVO> result = categoryService.listAllChildren("top1");
        assertThat(result).hasSize(2);
        assertThat(result.stream().map(CategoryVO::getId)).containsExactly("sub1_1", "sub1_2");
    }

    /**
     * 验证查询非第一个顶级分类时能够正确返回其子分类
     */
    @Test
    @DisplayName("查询非首个顶级分类时应能遍历并返回对应子分类")
    void listAllChildren_whenParentIdIsSecondTopCategory_returnsSecondChildren() {
        List<CategoryVO> result = categoryService.listAllChildren("top2");
        assertThat(result).hasSize(1);
        assertThat(result.get(0).getId()).isEqualTo("sub2_1");
    }

    /**
     * 验证能够遍历深入到非首个子分支并正确返回其子节点
     */
    @Test
    @DisplayName("查询深层非首个子分支时应深入正确分支并返回其子节点")
    void listAllChildren_whenParentIdInDeepSecondBranch_returnsCorrectChildren() {
        List<CategoryVO> result = categoryService.listAllChildren("sub1_2");
        assertThat(result).hasSize(1);
        assertThat(result.get(0).getId()).isEqualTo("leaf1_2_1");
    }

    /**
     * 验证当未找到目标节点时应返回空列表而不是将整个列表原样返回
     */
    @Test
    @DisplayName("分类节点不存在时应返回空列表")
    void listAllChildren_whenParentIdNotFound_returnsEmptyList() {
        List<CategoryVO> result = categoryService.listAllChildren("not_exist_id");
        assertThat(result).isNotNull().isEmpty();
    }
}
