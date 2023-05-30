package cn.lili.modules.page.service;

import cn.lili.modules.page.entity.dos.Article;
import cn.lili.modules.page.entity.dto.ArticleSearchParams;
import cn.lili.modules.page.entity.vos.ArticleVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;

import java.util.List;


/**
 * 文章业务层
 *
 * @author pikachu
 * @since 2020/11/18 11:40 上午
 */
@CacheConfig(cacheNames = "{article}")
public interface ArticleService extends IService<Article> {

    /**
     * 管理端获取文章
     * @param articleSearchParams
     * @return
     */
    IPage<ArticleVO> managerArticlePage(ArticleSearchParams articleSearchParams);
    /**
     * 获取文章分页
     *
     * @param articleSearchParams 文章搜索条件
     * @return 文章分页
     */
    IPage<ArticleVO> articlePage(ArticleSearchParams articleSearchParams);

    /**
     * 获取文章分页
     *
     * @param categoryId 文章分类ID
     * @return 文章分页
     */
    List<Article> list(String categoryId);

    /**
     * 修改文章内容
     *
     * @param article 文章
     * @return 修改后的文章
     */
    @CacheEvict(key = "#article.id")
    Article updateArticle(Article article);

    /**
     * 删除文章--id
     *
     * @param id
     */
    @CacheEvict(key = "#id")
    void customRemove(String id);

    /**
     * 读取文章
     *
     * @param id
     * @return 文章
     */
    @Cacheable(key = "#id")
    Article customGet(String id);

    /**
     * 读取文章
     *
     * @param type
     * @return 文章
     */
    @Cacheable(key = "#type")
    Article customGetByType(String type);

    /**
     * 修改文章状态
     *
     * @param id     文章ID
     * @param status 显示状态
     * @return 操作状态
     */
    @CacheEvict(key = "#id")
    Boolean updateArticleStatus(String id, boolean status);

    /**
     * 修改文章--文章类型修改
     * @param article
     * @return
     */
    @CacheEvict(key = "#article.type")
    Article updateArticleType(Article article);
}