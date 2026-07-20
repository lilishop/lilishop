package cn.lili.modules.member.entity.dos;


import cn.lili.common.security.sensitive.Sensitive;
import cn.lili.common.security.sensitive.enums.SensitiveStrategy;
import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 客户积分历史
 *
 * @author Bulbasaur
 * @since 2020-02-25 14:10:16
 */
@Data
@EqualsAndHashCode(callSuper = false)
@TableName("li_member_points_history")
@Schema(description = "客户积分历史")
public class MemberPointsHistory extends BaseIdEntity {

    private static final long serialVersionUID = 1L;

    @CreatedBy
    @TableField(fill = FieldFill.INSERT)
    @Schema(description = "创建者", hidden = true)
    private String createBy;

    @CreatedDate
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @TableField(fill = FieldFill.INSERT)
    @Schema(description = "创建时间", hidden = true)
    private Date createTime;

    @Schema(description = "客户ID")
    private String memberId;


    @Sensitive(strategy = SensitiveStrategy.PHONE)
    @Schema(description = "客户名称")
    private String memberName;

    @Schema(description = "当前积分")
    private Long point;

    @Schema(description = "消费之前积分")
    private Long beforePoint;

    @Schema(description = "变动积分")
    private Long variablePoint;

    @Schema(description = "content")
    private String content;

    /**
     * @see cn.lili.modules.member.entity.enums.PointTypeEnum
     */
    @Schema(description = "积分类型")
    private String pointType;

    /**
     * @see cn.lili.modules.member.entity.enums.PointSourceEnum
     */
    @Schema(description = "积分来源")
    private String pointSource;

}