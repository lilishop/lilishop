package cn.lili.modules.order.order.entity.vo;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.List;

/**
 * 交易投诉 参数
 *
 * @author paulG
 * @since 2020/12/4
 **/
@Data
public class OrderComplaintOperationParams {


    @ApiModelProperty(value = "要更改的状态状态")
    private String complainStatus;

    @ApiModelProperty("交易投诉主键")
    private String complainId;

    @ApiModelProperty("商家申诉内容")
    private String appealContent;

    @ApiModelProperty("商家申诉上传的图片")
    private List<String> images;

    @ApiModelProperty("仲裁结果")
    private String arbitrationResult;

}
