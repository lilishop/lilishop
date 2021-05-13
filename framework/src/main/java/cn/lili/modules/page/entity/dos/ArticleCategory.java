package cn.lili.modules.page.entity.dos;

import cn.lili.base.BaseEntity;
import cn.lili.modules.page.entity.enums.ArticleCategoryEnum;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Entity;
import javax.persistence.Table;
import javax.validation.constraints.NotEmpty;

/**
 * 文章分类
 *
 * @author pikachu
 * @author Bulbasaur
 * @date 2020/12/10 17:42
 */
@Data
@Entity
@Table(name = "li_article_category")
@TableName("li_article_category")
@ApiModel(value = "文章分类")
@NoArgsConstructor
@AllArgsConstructor
public class ArticleCategory extends BaseEntity {

    private static final long serialVersionUID = 1L;


    @ApiModelProperty(value = "分类名称")
    @NotEmpty(message = "分类名称不能为空")
    private String articleCategoryName;

    @ApiModelProperty(value = "父分类ID")
    private String parentId;

    @ApiModelProperty(value = "排序，正序123")
    private Integer sort;

    @ApiModelProperty(value = "层级")
    private Integer level;

    /**
     * @see ArticleCategoryEnum
     */
    @ApiModelProperty(value = "类型")
    private String type;

}