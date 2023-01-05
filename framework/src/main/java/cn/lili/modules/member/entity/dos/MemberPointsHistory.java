package cn.lili.modules.member.entity.dos;


import cn.lili.common.security.sensitive.Sensitive;
import cn.lili.common.security.sensitive.enums.SensitiveStrategy;
import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 会员积分历史
 *
 * @author Bulbasaur
 * @since 2020-02-25 14:10:16
 */
@Data
@TableName("li_member_points_history")
@ApiModel(value = "会员积分历史")
public class MemberPointsHistory extends BaseIdEntity {

    private static final long serialVersionUID = 1L;

    @CreatedBy
    @TableField(fill = FieldFill.INSERT)
    @ApiModelProperty(value = "创建者", hidden = true)
    private String createBy;

    @CreatedDate
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @TableField(fill = FieldFill.INSERT)
    @ApiModelProperty(value = "创建时间", hidden = true)
    private Date createTime;

    @ApiModelProperty(value = "会员ID")
    private String memberId;


    @Sensitive(strategy = SensitiveStrategy.PHONE)
    @ApiModelProperty(value = "会员名称")
    private String memberName;

    @ApiModelProperty(value = "当前积分")
    private Long point;

    @ApiModelProperty(value = "消费之前积分")
    private Long beforePoint;

    @ApiModelProperty(value = "变动积分")
    private Long variablePoint;

    @ApiModelProperty(value = "content")
    private String content;

    /**
     * @see cn.lili.modules.member.entity.enums.PointTypeEnum
     */
    @ApiModelProperty(value = "积分类型")
    private String pointType;

}