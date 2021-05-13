package cn.lili.modules.member.entity.dos;

import cn.lili.base.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 会员消息
 *
 * @author Chopper
 * @date 2020-02-25 14:10:16
 */
@Data
@Entity
@Table(name = "li_member_notice_senter")
@TableName("li_member_notice_senter")
@ApiModel(value = "会员消息")
public class MemberNoticeSenter extends BaseEntity {
    /**
     * 标题
     */
    @Column(name = "title")
    @ApiModelProperty(value = "标题")
    private String title;
    /**
     * 消息内容
     */
    @Column(name = "content")
    @ApiModelProperty(value = "消息内容")
    private String content;
    /**
     * 会员id
     */
    @Column(name = "member_ids")
    @ApiModelProperty(value = "会员id")
    private String memberIds;
    /**
     * 发送类型
     */
    @Column(name = "send_type")
    @ApiModelProperty(value = "发送类型,ALL 全站，SELECT 指定会员")
    private String sendType;

}