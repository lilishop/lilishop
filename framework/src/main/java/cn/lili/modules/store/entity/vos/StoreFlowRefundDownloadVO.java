package cn.lili.modules.store.entity.vos;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 店铺流水下载
 *
 * @author Bulbasaur
 * @date: 2021/8/13 4:14 下午
 */
@Data
public class StoreFlowRefundDownloadVO extends StoreFlowPayDownloadVO {

    @ApiModelProperty(value = "售后SN")
    private String refundSn;

}
