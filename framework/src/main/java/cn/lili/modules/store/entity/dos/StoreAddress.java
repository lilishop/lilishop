package cn.lili.modules.store.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.NotEmpty;

/**
 * 店铺自提点
 *
 * @author Bulbasaur
 * @since 2020/12/7 15:09
 */
@Data
@TableName("li_store_address")
@ApiModel(value = "店铺自提点")
public class StoreAddress extends BaseEntity {

    @ApiModelProperty(value = "店铺id", hidden = true)
    private String storeId;

    @NotEmpty
    @ApiModelProperty(value = "自提点名称")
    private String addressName;

    @ApiModelProperty(value = "经纬度")
    @NotEmpty
    private String center;

    @ApiModelProperty(value = "地址")
    private String address;

    @ApiModelProperty(value = "电话")
    private String mobile;

}
