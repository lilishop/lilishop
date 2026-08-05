package cn.lili.modules.distribution.entity.vos;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 分销员端客户列表项
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Data
public class DistributionBuyerCustomerVO {

    @Schema(description = "关系ID")
    private String id;

    @Schema(description = "客户会员ID")
    private String memberId;

    @Schema(description = "客户昵称")
    private String memberNickname;

    @Schema(description = "客户头像")
    private String memberAvatar;

    @Schema(description = "客户手机号")
    private String memberMobile;

    @Schema(description = "关系状态")
    private String relationStatus;

    @Schema(description = "状态文案")
    private String relationStatusText;

    @Schema(description = "是否有效")
    private Boolean relationEffective;

    @Schema(description = "是否星标")
    private Boolean starred;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "绑定时间")
    private Date bindTime;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "到期时间")
    private Date expireAt;
}
