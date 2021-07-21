package cn.lili.modules.goods.entity.vos;

import cn.hutool.core.bean.BeanUtil;
import cn.lili.modules.goods.entity.dos.Brand;
import cn.lili.modules.goods.entity.dos.Category;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.util.Comparator;
import java.util.Date;
import java.util.List;

/**
 * 分类VO
 *
 * @author paulG
 * @since 2020/12/1
 **/
@Data
@NoArgsConstructor
@AllArgsConstructor
public class CategoryVO extends Category {

    private static final long serialVersionUID = 3775766246075838410L;

    @ApiModelProperty(value = "父节点名称")
    private String parentTitle;

    @ApiModelProperty("子分类列表")
    private List<CategoryVO> children;

    @ApiModelProperty("分类关联的品牌列表")
    private List<Brand> brandList;

    public CategoryVO(Category category) {
        BeanUtil.copyProperties(category, this);
    }

    public CategoryVO(String id, String createBy, Date createTime, String updateBy, Date updateTime, Boolean deleteFlag, String name, String parentId, Integer level, BigDecimal sortOrder, Double commissionRate, String image, Boolean supportChannel) {
        super(id, createBy, createTime, updateBy, updateTime, deleteFlag, name, parentId, level, sortOrder, commissionRate, image, supportChannel);
    }

    public List<CategoryVO> getChildren() {

        if (children != null) {
            children.sort(new Comparator<CategoryVO>() {
                @Override
                public int compare(CategoryVO o1, CategoryVO o2) {
                    return o1.getSortOrder().compareTo(o2.getSortOrder());
                }
            });
            return children;
        }
        return null;
    }
}
