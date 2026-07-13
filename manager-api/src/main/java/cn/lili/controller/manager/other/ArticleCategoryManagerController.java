package cn.lili.controller.manager.other;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.page.entity.dos.ArticleCategory;
import cn.lili.modules.page.entity.vos.ArticleCategoryVO;
import cn.lili.modules.page.service.ArticleCategoryService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.util.List;

/**
 * 管理端,文章分类管理接口
 *
 * @author pikachu
 * @since 2020-05-5 15:10:16
 */
@Slf4j
@RestController
@Tag(name = "管理端,文章分类管理接口")
@RequestMapping("/manager/other/articleCategory")
public class ArticleCategoryManagerController {

    /**
     * 文章分类
     */
    @Autowired
    private ArticleCategoryService articleCategoryService;

    @Operation(summary = "查询分类列表")
    @GetMapping("/all-children")
    public ResultMessage<List<ArticleCategoryVO>> allChildren() {
        try {
            return ResultUtil.data(this.articleCategoryService.allChildren());
        } catch (Exception e) {
            log.error("查询分类列表错误", e);
        }
        return null;
    }

    @Operation(summary = "查看文章分类")
    @Parameter(name = "id", description = "文章分类ID", required = true)
    @GetMapping("/{id}")
    public ResultMessage<ArticleCategory> getArticleCategory(
            @PathVariable String id) {
        return ResultUtil.data(this.articleCategoryService.getById(id));
    }

    @Operation(summary = "保存文章分类")
    @Parameter(name = "articleCategory", description = "文章分类信息", required = true)
    @PostMapping
    public ResultMessage<ArticleCategory> save(@Valid ArticleCategory articleCategory) {
        if (articleCategory.getLevel() == null) {
            articleCategory.setLevel(0);
        }
        if (articleCategory.getSort() == null) {
            articleCategory.setSort(0);
        }

        return ResultUtil.data(articleCategoryService.saveArticleCategory(articleCategory));
    }

    @Operation(summary = "修改文章分类")
    @PutMapping("/update/{id}")
    @Parameter(name = "articleCategory", description = "文章分类信息", required = true)
    @Parameter(name = "id", description = "文章分类ID", required = true)
    public ResultMessage<ArticleCategory> update(@Valid ArticleCategory articleCategory, 
            @PathVariable("id") String id) {

        if (articleCategory.getLevel() == null) {
            articleCategory.setLevel(0);
        }
        if (articleCategory.getSort() == null) {
            articleCategory.setSort(0);
        }

        articleCategory.setId(id);
        return ResultUtil.data(articleCategoryService.updateArticleCategory(articleCategory));
    }

    @Operation(summary = "删除文章分类")
    @Parameter(name = "id", description = "文章分类ID", required = true)
    @DeleteMapping("/{id}")
    public ResultMessage<ArticleCategory> deleteById(
            @PathVariable String id) {
        articleCategoryService.deleteById(id);
        return ResultUtil.success();
    }
}
