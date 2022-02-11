package cn.lili.modules.member.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;


/**
 * 会员站内信
 *
 * @author Chopper
 * @since 2020-02-25 14:10:16
 */
@Data
@TableName("li_member_notice")
@ApiModel(value = "会员站内信")
public class MemberNotice extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "会员id")
    private String memberId;

    @ApiModelProperty(value = "是否已读")
    private Boolean isRead;

    @ApiModelProperty(value = "阅读时间")
    private Long receiveTime;

    @ApiModelProperty(value = "标题")
    private String title;

    @ApiModelProperty(value = "站内信内容")
    private String content;

}