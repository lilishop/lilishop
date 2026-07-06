package cn.lili.modules.live.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
@Data
@NoArgsConstructor
@Schema(title = "直播间")
@TableName("li_live_room")
public class LiveRoom extends BaseEntity {

    @Schema(title = "直播标题")
    private String title;

    @Schema(title = "直播描述")
    private String description;

    @Schema(title = "直播封面")
    private String coverImg;

    @Schema(title = "展示模式: 0 横屏 1 竖屏 2 三分屏")
    private String displayMode;

    @Schema(title = "直播观看人数")
    private Integer viewerCount;

    @Schema(title = "真实直播观看人数")
    private Long actualViewNumber = 0L;

    @Schema(title = "直播介绍")
    private String liveIntroduce;

    @Schema(title = "直播点赞数")
    private Integer totalLikes = 0;

    @Schema(title = "直播评论数")
    private Integer totalComments = 0;

    @Schema(title = "评论人数")
    private Integer commentPeopleNumber = 0;

    @Schema(title = "推流服务器")
    private String pushStreamServer;

    @Schema(title = "推流码")
    private String pushStreamCode;

    @Schema(title = "拉流地址")
    private String pullStreamUrl;

    @Schema(title = "直播开始时间")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date startTime;

    @Schema(title = "直播结束时间")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date endTime;

    @Schema(title = "IM群Id")
    private String imGroupId;

    @Schema(title = "订单数")
    private Integer orderCount = 0;

    @Schema(title = "商品数")
    private Integer goodsCount  = 0;

    @Schema(title = "店铺Id")
    private String storeId;

    @Schema(title = "店铺名称")
    private String storeName;

    @Schema(title = "审核标识 是否审核评论")
    private Boolean authFlag;

    @Schema(title = "直播状态")
    private String liveStatus;

    @Schema(title = "直播编号")
    private String sn;

    public Boolean getAuthFlag(){
        if(authFlag == null){
            return false;
        }
        return authFlag;
    }
}
