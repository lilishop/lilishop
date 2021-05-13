package cn.lili.modules.goods.entity.dto;

import cn.lili.common.utils.StringUtils;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 规格搜索参数
 *
 * @author paulG
 * @date 2020/12/19
 **/
@Data
public class SpecificationSearchParams {


    @ApiModelProperty(value = "规格名")
    private String specName;

    @ApiModelProperty(value = "绑定分类")
    private String categoryPath;

    @ApiModelProperty(value = "未删除 ")
    private Boolean deleteFlag;

    public <T> QueryWrapper<T> queryWrapper() {
        QueryWrapper<T> queryWrapper = new QueryWrapper<>();
        queryWrapper.like(StringUtils.isNotEmpty(specName), "spec_name", specName);
        queryWrapper.eq(deleteFlag != null, "delete_flag", deleteFlag);
        return queryWrapper;
    }

}
