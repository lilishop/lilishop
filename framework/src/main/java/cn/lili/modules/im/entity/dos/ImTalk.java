package cn.lili.modules.im.entity.dos;


import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.mybatis.BaseTenantEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * @author Chopper
 */
@Data
@TableName("li_im_talk")
@ApiModel(value = "聊天")
@NoArgsConstructor
@AllArgsConstructor
public class ImTalk extends BaseTenantEntity {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("用户1 id")
    private String userId1;

    @ApiModelProperty("用户2 id")
    private String userId2;

    @ApiModelProperty("用户1置顶")
    private Boolean top1;

    @ApiModelProperty("用户2置顶")
    private Boolean top2;

    @ApiModelProperty("用户1 不可见")
    private Boolean disable1;

    @ApiModelProperty("用户2 不可见")
    private Boolean disable2;

    @ApiModelProperty("用户1名字")
    private String name1;

    @ApiModelProperty("用户2名字")
    private String name2;

    @ApiModelProperty("用户1头像")
    private String face1;

    @ApiModelProperty("用户2头像")
    private String face2;

    @ApiModelProperty("用户1的店铺标识")
    private Boolean storeFlag1;

    @ApiModelProperty("用户2的店铺标识")
    private Boolean storeFlag2;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @ApiModelProperty(value = "最后聊天时间", hidden = true)
    private Date lastTalkTime;

    @ApiModelProperty(value = "最后聊天内容")
    private String lastTalkMessage;

    @ApiModelProperty(value = "最后发送消息类型")
    private String lastMessageType;

    @ApiModelProperty(value = "坐席Id")
    private String tenantId;

    @ApiModelProperty(value = "坐席名称")
    private String tenantName;


    public ImTalk(String userId1, String userId2,
                  String face1, String face2,
                  String name1, String name2
    ) {
        this.userId1 = userId1;
        this.userId2 = userId2;
        this.top1 = false;
        this.top2 = false;
        this.disable1 = false;
        this.disable2 = false;
        this.storeFlag1 = false;
        this.storeFlag2 = false;
        this.setId(SnowFlake.getIdStr());
        this.lastTalkTime = new Date();
        this.face1 = face1;
        this.face2 = face2;
        this.name1 = name1;
        this.name2 = name2;
    }

    public ImTalk(Member member1,Member member2){
        if(Long.parseLong(member1.getId()) > Long.parseLong(member2.getId())){
            this.userId1 = member2.getId();
            this.userId2 = member1.getId();
            this.top1 = false;
            this.top2 = false;
            this.disable1 = false;
            this.disable2 = false;
            this.storeFlag1 = false;
            this.storeFlag2 = false;
            this.setId(SnowFlake.getIdStr());
            this.lastTalkTime = new Date();
            this.face1 = member2.getFace();
            this.face2 = member1.getFace();
            this.name1 = member2.getNickName();
            this.name2 = member1.getNickName();
        }else{
            this.userId1 = member1.getId();
            this.userId2 = member2.getId();
            this.top1 = false;
            this.top2 = false;
            this.disable1 = false;
            this.disable2 = false;
            this.storeFlag1 = false;
            this.storeFlag2 = false;
            this.setId(SnowFlake.getIdStr());
            this.lastTalkTime = new Date();
            this.face1 = member1.getFace();
            this.face2 = member2.getFace();
            this.name1 = member1.getNickName();
            this.name2 = member2.getNickName();
        }
    }
    public ImTalk(Member member, Store store){
        if(Long.parseLong(member.getId()) > Long.parseLong(store.getId())){
            this.userId1 = store.getId();
            this.userId2 = member.getId();
            this.top1 = false;
            this.top2 = false;
            this.disable1 = false;
            this.disable2 = false;
            this.storeFlag1 = true;
            this.storeFlag2 = false;
            this.setId(SnowFlake.getIdStr());
            this.lastTalkTime = new Date();
            this.face1 = store.getStoreLogo();
            this.face2 = member.getFace();
            this.name1 = store.getStoreName();
            this.name2 = member.getNickName();
        }else{
            this.userId1 = member.getId();
            this.userId2 = store.getId();
            this.top1 = false;
            this.top2 = false;
            this.disable1 = false;
            this.disable2 = false;
            this.storeFlag1 = false;
            this.storeFlag2 = true;
            this.setId(SnowFlake.getIdStr());
            this.lastTalkTime = new Date();
            this.face1 = member.getFace();
            this.face2 = store.getStoreLogo();
            this.name1 = member.getNickName();
            this.name2 = store.getStoreName();
        }
    }
}