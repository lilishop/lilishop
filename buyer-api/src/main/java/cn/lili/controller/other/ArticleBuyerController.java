package cn.lili.controller.other;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.page.entity.dos.Article;
import cn.lili.modules.page.entity.dto.ArticleSearchParams;
import cn.lili.modules.page.entity.vos.ArticleCategoryVO;
import cn.lili.modules.page.entity.vos.ArticleVO;
import cn.lili.modules.page.service.ArticleCategoryService;
import cn.lili.modules.page.service.ArticleService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.constraints.NotNull;
import java.util.List;

/**
 * 买家端,文章接口
 *
 * @author Chopper
 * @since 2020/11/16 10:02 下午
 */
@RestController
@Api(tags = "买家端,文章接口")
@RequestMapping("/buyer/other/article")
public class ArticleBuyerController {

    /**
     * 文章
     */
    @Autowired
    private ArticleService articleService;

    /**
     * 文章分类
     */
    @Autowired
    private ArticleCategoryService articleCategoryService;

    @ApiOperation(value = "获取文章分类列表")
    @GetMapping(value = "/articleCategory/list")
    public ResultMessage<List<ArticleCategoryVO>> getArticleCategoryList() {
        return ResultUtil.data(articleCategoryService.allChildren());
    }

    @ApiOperation(value = "分页获取")
    @GetMapping
    public ResultMessage<IPage<ArticleVO>> getByPage(ArticleSearchParams articleSearchParams) {
        return ResultUtil.data(articleService.articlePage(articleSearchParams));
    }

    @ApiOperation(value = "通过id获取文章")
    @ApiImplicitParam(name = "id", value = "文章ID", required = true, paramType = "path")
    @GetMapping(value = "/get/{id}")
    public ResultMessage<Article> get(@NotNull(message = "文章ID") @PathVariable("id") String id) {
        return ResultUtil.data(articleService.customGet(id));
    }

    @ApiOperation(value = "通过类型获取文章")
    @ApiImplicitParam(name = "type", value = "文章类型", required = true, paramType = "path")
    @GetMapping(value = "/type/{type}")
    public ResultMessage<Article> getByType(@NotNull(message = "文章类型") @PathVariable("type") String type) {
        return ResultUtil.data(articleService.customGetByType(type));
    }
}
