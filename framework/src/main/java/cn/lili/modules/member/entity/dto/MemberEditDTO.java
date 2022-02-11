package cn.lili.modules.member.entity.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.util.Date;

/**
 * 会员信息修改DTO
 *
 * @author Bulbasaur
 * @since 2020/12/11 14:39
 */
@Data
public class MemberEditDTO {


    @ApiModelProperty(value = "昵称", required = true)
    @Size(min = 2, max = 20, message = "会员昵称必须为2到20位之间")
    private String nickName;

    @ApiModelProperty(value = "会员地址ID")
    private String regionId;

    @ApiModelProperty(value = "会员地址")
    private String region;

    @Min(message = "必须为数字且1为男,0为女", value = 0)
    @Max(message = "必须为数字且1为男,0为女", value = 1)
    @NotNull(message = "会员性别不能为空")
    @ApiModelProperty(value = "会员性别,1为男，0为女", required = true)
    private Integer sex;

    @JsonFormat(pattern = "yyyy-MM-dd", timezone = "GMT+8")
  @DateTimeFormat(pattern = "yyyy-MM-dd")
    @ApiModelProperty(value = "会员生日")
    private Date birthday;

    @ApiModelProperty(value = "详细地址")
    private String address;

    @ApiModelProperty(value = "会员头像")
    private String face;

}
