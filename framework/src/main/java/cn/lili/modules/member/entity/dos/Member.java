package cn.lili.modules.member.entity.dos;

import cn.lili.common.enums.ClientTypeEnum;
import cn.lili.common.security.sensitive.Sensitive;
import cn.lili.common.security.sensitive.enums.SensitiveStrategy;
import cn.lili.common.utils.CommonUtil;
import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotEmpty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 客户
 *
 * @author Bulbasaur
 * @since 2020-02-25 14:10:16
 */
@Data
@TableName("li_member")
@Schema(description = "客户")
@NoArgsConstructor
@EqualsAndHashCode(callSuper = false)
public class Member extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @Schema(description = "客户用户名")
    private String username;

    // 密码散列只允许反序列化写入（注册/改密），任何接口都不序列化返回
    @JsonProperty(access = JsonProperty.Access.WRITE_ONLY)
    @Schema(description = "客户密码")
    private String password;

    @Schema(description = "昵称")
    private String nickName;

    @Min(message = "客户性别参数错误", value = 0)
    @Schema(description = "客户性别,1为男，0为女")
    private Integer sex;

    @JsonFormat(pattern = "yyyy-MM-dd", timezone = "GMT+8")
    @DateTimeFormat(pattern = "yyyy-MM-dd")
    @Schema(description = "客户生日")
    private Date birthday;

    @Schema(description = "客户地址ID")
    private String regionId;

    @Schema(description = "客户地址")
    private String region;

    @NotEmpty(message = "手机号码不能为空")
    @Schema(description = "手机号码", requiredMode = Schema.RequiredMode.REQUIRED)
    @Sensitive(strategy = SensitiveStrategy.PHONE)
    private String mobile;

    @Min(message = "必须为数字", value = 0)
    @Schema(description = "积分数量")
    private Long point;

    @Min(message = "必须为数字", value = 0)
    @Schema(description = "积分总数量")
    private Long totalPoint;

    @Schema(description = "客户头像")
    private String face;

    @Schema(description = "客户状态")
    private Boolean disabled;

    @Schema(description = "是否开通店铺")
    private Boolean haveStore;

    @Schema(description = "店铺ID")
    private String storeId;

    /**
     * @see ClientTypeEnum
     */
    @Schema(description = "客户端")
    private String clientEnum;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "最后一次登录时间")
    private Date lastLoginDate;

    @Schema(description = "客户等级ID")
    private String gradeId;

    @Min(message = "必须为数字", value = 0)
    @Schema(description = "经验值数量")
    private Long experience;


    public Member(String username, String password, String mobile) {
        this.username = username;
        this.password = password;
        this.mobile = mobile;
        this.nickName = CommonUtil.getSpecialStr("用户");
        this.disabled = true;
        this.haveStore = false;
        this.sex = 0;
        this.point = 0L;
        this.totalPoint = 0L;
        this.lastLoginDate = new Date();
    }

    public Member(String username, String password, String face, String nickName, Integer sex, String mobile) {
        this.username = username;
        this.password = password;
        this.mobile = mobile;
        this.nickName = nickName;
        this.disabled = true;
        this.haveStore = false;
        this.face = face;
        this.sex = sex;
        this.point = 0L;
        this.totalPoint = 0L;
        this.lastLoginDate = new Date();
    }
}
