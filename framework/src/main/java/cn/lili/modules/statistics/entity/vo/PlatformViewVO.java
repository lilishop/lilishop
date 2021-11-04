package cn.lili.modules.statistics.entity.vo;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.Date;

/**
 * 流量数据展示VO
 *
 * @author Chopper
 * @since 2020-06-19 17:50
 */
@Data
public class PlatformViewVO {

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd")
    @ApiModelProperty(value = "展示时间")
    private Date date;

    @ApiModelProperty(value = "pv数量")
    private Long pvNum;

    @ApiModelProperty(value = "uv数量")
    private Long uvNum;

    @ApiModelProperty(value = "店铺id")
    private String storeId = "-1";


    public PlatformViewVO() {
        //初始化参数
        pvNum = 0L;
        uvNum = 0L;
    }

    public Long getPvNum() {
        if(pvNum==null){
            return 0L;
        }
        return pvNum;
    }

    public Long getUvNum() {
        if(uvNum==null){
            return 0L;
        }
        return uvNum;
    }

    public PlatformViewVO(Date date) {
        //初始化参数
        pvNum = 0L;
        uvNum = 0L;
        this.date = date;
    }

}
