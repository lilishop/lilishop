package cn.lili.modules.system.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;


/**
 * 服务订阅消息
 * @author Chopper
 * @since 2020-02-25 14:10:16
 */
@Data
@TableName("li_service_notice")
@ApiModel(value = "服务订阅消息")
public class ServiceNotice extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "商家id，为-1时，代表是平台发布的消息")
    private String storeId;

    @ApiModelProperty(value = "banner图")
    private String bannerImage;

    @ApiModelProperty(value = "标题")
    private String title;

    @ApiModelProperty(value = "副标题")
    private String subTitle;

    @ApiModelProperty(value = "点击跳转（此内容与站内信内容只能有一个生效）")
    private String toUrl;

    @ApiModelProperty(value = "站内信内容(富文本框编辑，可以上传图片的html)")
    private String content;

}