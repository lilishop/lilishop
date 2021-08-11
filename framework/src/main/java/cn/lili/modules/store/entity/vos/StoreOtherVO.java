package cn.lili.modules.store.entity.vos;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 店铺其他信息
 * @author Bulbasaur
 * @date: 2021/8/11 3:42 下午
 *
 */
@Data
public class StoreOtherVO {

    @ApiModelProperty(value = "公司名称")
    private String companyName;

    @ApiModelProperty(value = "公司地址")
    private String companyAddress;

    @ApiModelProperty(value = "公司地址地区")
    private String companyAddressPath;

    @ApiModelProperty(value = "营业执照电子版")
    private String licencePhoto;

    @ApiModelProperty(value = "法定经营范围")
    private String scope;

    @ApiModelProperty(value = "员工总数")
    private Integer employeeNum;
}
