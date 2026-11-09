package cn.lili.modules.page.serviceimpl;

import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.modules.page.entity.dos.ArticleCategory;
import cn.lili.modules.page.entity.vos.ArticleCategoryVO;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.verify;

/**
 * 文章分类树回归测试，验证历史空父分类 ID 不影响正常分类及缓存生成。
 *
 * @author Bulbasaur
 * @since 2026-11-09
 */
@ExtendWith(MockitoExtension.class)
class ArticleCategoryServiceImplTest {

    @Mock
    private Cache cache;

    @Spy
    @InjectMocks
    private ArticleCategoryServiceImpl service;

    /** 验证空父 ID 的根分类仍保留，孤立子分类不挂载，正常父子关系和排序不变。 */
    @Test
    void nullParentDoesNotPreventBuildingAndCachingTree() {
        ArticleCategory root = category("root", null, 0, 2);
        ArticleCategory firstRoot = category("first-root", "0", 0, 1);
        ArticleCategory child = category("child", "root", 1, 2);
        ArticleCategory firstChild = category("first-child", "root", 1, 1);
        ArticleCategory orphan = category("orphan", null, 1, 0);
        doReturn(List.of(root, child, orphan, firstChild, firstRoot)).when(service).list();

        List<ArticleCategoryVO> tree = service.allChildren();

        assertEquals(List.of("first-root", "root"), tree.stream().map(ArticleCategory::getId).toList());
        assertEquals(List.of("first-child", "child"),
                tree.get(1).getChildren().stream().map(ArticleCategory::getId).toList());
        assertEquals(0, tree.get(0).getChildren().size());
        assertEquals(0, tree.get(1).getChildren().get(0).getChildren().size());
        verify(cache).put(CachePrefix.ARTICLE_CATEGORY.getPrefix(), tree);
    }

    /** 验证命中已有分类缓存时仍返回原缓存，不改变正常缓存读取行为。 */
    @Test
    void returnsExistingCachedTree() {
        List<ArticleCategoryVO> cached = List.of(new ArticleCategoryVO(category("root", null, 0, 0)));
        doReturn(cached).when(cache).get(CachePrefix.ARTICLE_CATEGORY.getPrefix());

        assertSame(cached, service.allChildren());
    }

    /** 构造指定层级、父节点及排序的分类记录，模拟数据库历史数据。 */
    private ArticleCategory category(String id, String parentId, int level, int sort) {
        ArticleCategory category = new ArticleCategory();
        category.setId(id);
        category.setParentId(parentId);
        category.setLevel(level);
        category.setSort(sort);
        return category;
    }
}
