package cn.lili.modules.page.entity.vos;

import cn.lili.common.utils.BeanUtil;
import cn.lili.modules.page.entity.dos.ArticleCategory;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

/**
 * 文章分类VO
 *
 * @author Chopper
 * @since 2021-03-26 11:32
 */
@Data
public class ArticleCategoryVO extends ArticleCategory {

    @ApiModelProperty(value = "子菜单")
    private List<ArticleCategoryVO> children = new ArrayList<>();

    public ArticleCategoryVO() {

    }

    public ArticleCategoryVO(ArticleCategory articleCategory) {
        BeanUtil.copyProperties(articleCategory, this);
    }

    public List<ArticleCategoryVO> getChildren() {
        if (children != null) {
            children.sort(new Comparator<ArticleCategoryVO>() {
                @Override
                public int compare(ArticleCategoryVO o1, ArticleCategoryVO o2) {
                    return o1.getSort().compareTo(o2.getSort());
                }
            });
            return children;
        }
        return null;
    }
}
