package cn.lili.modules.im.entity.dos;


import cn.lili.common.utils.SnowFlake;
import cn.lili.mybatis.BaseTenantEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * @author Chopper
 */
@Data
@TableName("li_im_talk")
@ApiModel(value = "聊天")
public class ImTalk extends BaseTenantEntity {

    private static final long serialVersionUID = 1L;

    /**
     * 用户1 id小的排在1
     */
    private String userId1;
    /**
     * 用户1 id大的排在2
     */
    private String userId2;

    /**
     * 用户1置顶
     */
    private Boolean top1;

    /**
     * 用户2置顶
     */
    private Boolean top2;
    /**
     * 用户1 不可见
     */
    private Boolean disable1;

    /**
     * 用户2 不可见
     */
    private Boolean disable2;
    /**
     * 用户1名字
     */
    private String name1;

    /**
     * 用户2名字
     */
    private String name2;
    /**
     * 用户1头像
     */
    private String face1;

    /**
     * 用户2头像
     */
    private String face2;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @ApiModelProperty(value = "最后聊天时间", hidden = true)
    private Date lastTalkTime;

    public ImTalk() {

    }

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
        this.setId(SnowFlake.getIdStr());
        this.lastTalkTime = new Date();
        this.face1 = face1;
        this.face2 = face2;
        this.name1 = name1;
        this.name2 = name2;
    }
}