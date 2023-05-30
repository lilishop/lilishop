package cn.lili.controller.other;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.page.entity.dos.Article;
import cn.lili.modules.page.entity.dto.ArticleSearchParams;
import cn.lili.modules.page.entity.vos.ArticleVO;
import cn.lili.modules.page.service.ArticleService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 店铺端,文章接口
 *
 * @author pikachu
 * @since 2020-05-06 15:18:56
 */
@RestController
@Api(tags = "店铺端,文章接口")
@RequestMapping("/store/other/article")
public class ArticleStoreController {

    /**
     * 文章
     */
    @Autowired
    private ArticleService articleService;

    @ApiOperation(value = "分页获取")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "categoryId", value = "文章分类ID", paramType = "query")
    })
    @GetMapping(value = "/getByPage")
    public ResultMessage<IPage<ArticleVO>> getByPage(ArticleSearchParams articleSearchParams) {
        return ResultUtil.data(articleService.articlePage(articleSearchParams));
    }

    @ApiOperation(value = "查看文章")
    @ApiImplicitParam(name = "id", value = "文章ID", required = true, paramType = "path")
    @GetMapping(value = "/{id}")
    public ResultMessage<Article> get(@PathVariable String id) {

        return ResultUtil.data(articleService.getById(id));
    }
}
