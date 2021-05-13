package cn.lili.modules.member.entity.dos;

import cn.lili.base.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import java.util.Date;

/**
 * 会员消息
 *
 * @author Chopper
 * @date 2020-02-25 14:10:16
 */
@Data
@Entity
@Table(name = "li_member_notice_log")
@TableName("li_member_notice_log")
@ApiModel(value = "会员消息")
public class MemberNoticeLog extends BaseEntity {

    private static final long serialVersionUID = 1L;
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
     * 管理员id
     */
    @Column(name = "admin_id")
    @ApiModelProperty(value = "管理员id")
    private String adminId;
    /**
     * 管理员名称
     */
    @Column(name = "admin_name")
    @ApiModelProperty(value = "管理员名称")
    private String adminName;
    /**
     * 发送时间
     */
    @Column(name = "send_time")
    @ApiModelProperty(value = "发送时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
    private Date sendTime;
    /**
     * 发送类型
     */
    @Column(name = "send_type")
    @ApiModelProperty(value = "发送类型,0全站，1指定会员")
    private Integer sendType;


}