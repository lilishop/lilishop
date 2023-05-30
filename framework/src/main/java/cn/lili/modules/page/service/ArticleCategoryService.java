package cn.lili.modules.page.service;

import cn.lili.modules.page.entity.dos.ArticleCategory;
import cn.lili.modules.page.entity.vos.ArticleCategoryVO;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;


/**
 * 文章分类业务层
 *
 * @author Bulbasaur
 * @since 2020/11/24 17:07
 */
public interface ArticleCategoryService extends IService<ArticleCategory> {

    /**
     * 添加文章分类
     *
     * @param articleCategory 文章分类
     * @return 文章分类
     */
    ArticleCategory saveArticleCategory(ArticleCategory articleCategory);

    /**
     * 修改文章分类
     *
     * @param articleCategory 文章分类
     * @return 文章分类
     */
    ArticleCategory updateArticleCategory(ArticleCategory articleCategory);

    /**
     * 查询所有的分类，父子关系
     *
     * @return 文章分类
     */
    List<ArticleCategoryVO> allChildren();

    /**
     * 删除文章分类
     *
     * @param id 文章分类id
     * @return 操作状态
     */
    boolean deleteById(String id);


}