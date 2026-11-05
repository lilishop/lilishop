package cn.lili.modules.member.entity.vo;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.io.Serializable;
import java.util.Date;

/**
 * 管理端：等级权益发放记录（会员达到等级时触发的一次性发放）
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
@Data
@Schema(description = "权益发放记录")
public class MemberBenefitGrantRecordVO implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "记录ID")
    private String id;

    @Schema(description = "会员ID")
    private String memberId;

    @Schema(description = "会员手机号")
    private String memberMobile;

    @Schema(description = "会员用户名")
    private String memberUsername;

    @Schema(description = "等级ID")
    private String gradeId;

    @Schema(description = "等级名称")
    private String gradeName;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "发放时间")
    private Date createTime;
}
