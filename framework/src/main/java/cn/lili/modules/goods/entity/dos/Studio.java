package cn.lili.modules.goods.entity.dos;

import cn.lili.modules.goods.entity.enums.StudioStatusEnum;
import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;


/**
 * 小程序直播间
 *
 * @author Bulbasaur
 * @since 2021/5/17 9:47 上午
 */
@Data
@ApiModel(value = "直播间")
@TableName("li_studio")
public class Studio extends BaseEntity {

    /**
     * 直播间名字，最短3个汉字，最长17个汉字，1个汉字相当于2个字符
     */
    @ApiModelProperty(value = "直播间名字")
    private String name;

    /**
     * 背景图，填入mediaID（mediaID获取后，三天内有效）；图片mediaID的获取，请参考以下文档： https://developers.weixin.qq.com/doc/offiaccount/Asset_Management/New_temporary_materials.html；直播间背景图，图片规则：建议像素1080*1920，大小不超过2M
     */
    @ApiModelProperty(value = "背景图")
    private String coverImg;

    /**
     * 直播计划开始时间（开播时间需要在当前时间的10分钟后 并且 开始时间不能在 6 个月后）
     */
    @ApiModelProperty(value = "开始时间")
    private String startTime;

    /**
     * 直播计划结束时间（开播时间和结束时间间隔不得短于30分钟，不得超过24小时）
     */
    @ApiModelProperty(value = "结束时间")
    private String endTime;

    /**
     * 主播昵称，最短2个汉字，最长15个汉字，1个汉字相当于2个字符
     */
    @ApiModelProperty(value = "主播昵称")
    private String anchorName;

    /**
     * 主播微信号，如果未实名认证，需要先前往“小程序直播”小程序进行实名验证, 小程序二维码链接：https://res.wx.qq.com/op_res/9rSix1dhHfK4rR049JL0PHJ7TpOvkuZ3mE0z7Ou_Etvjf-w1J_jVX0rZqeStLfwh
     */
    @ApiModelProperty(value = "主播微信号")
    private String anchorWechat;

    /**
     * 分享图，填入mediaID（mediaID获取后，三天内有效）；图片mediaID的获取，请参考以下文档： https://developers.weixin.qq.com/doc/offiaccount/Asset_Management/New_temporary_materials.html；直播间分享图，图片规则：建议像素800*640，大小不超过1M；
     */
    @ApiModelProperty(value = "分享图")
    private String shareImg;

    /**
     * 购物直播频道封面图，填入mediaID（mediaID获取后，三天内有效）；图片mediaID的获取，请参考以下文档： https://developers.weixin.qq.com/doc/offiaccount/Asset_Management/New_temporary_materials.html; 购物直播频道封面图，图片规则：建议像素800*800，大小不超过100KB；
     */
    @ApiModelProperty(value = "封面图")
    private String feedsImg;


    @ApiModelProperty(value = "回放视频链接")
    private String mediaUrl;

    @ApiModelProperty(value = "房间ID")
    private Integer roomId;

    @ApiModelProperty(value = "小程序直播码")
    private String qrCodeUrl;

    @ApiModelProperty(value = "店铺ID")
    private String storeId;

    @ApiModelProperty(value = "直播间商品数量")
    private Integer roomGoodsNum;

    @ApiModelProperty(value = "直播间商品(最多展示两个商品：name/goodsImage)")
    private String roomGoodsList;

    @ApiModelProperty(value = "推荐直播间")
    private boolean recommend;

    /**
     * @see StudioStatusEnum
     */
    @ApiModelProperty(value = "直播间状态")
    private String  status;
}
