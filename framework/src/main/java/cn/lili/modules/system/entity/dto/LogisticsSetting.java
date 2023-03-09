package cn.lili.modules.system.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import cn.lili.modules.logistics.entity.enums.LogisticsEnum;
import lombok.Data;

import java.io.Serializable;

/**
 * 快递设置
 *
 * @author Chopper
 * @since 2020-03-10 10:04 上午
 */
@Data
public class LogisticsSetting implements Serializable {
    private static final long serialVersionUID = 3520379500723173689L;

    /**
     * 快递查询类型
     * @see LogisticsEnum
     */
    private String type;

    /**
     * 企业id
     */
    private String kdniaoEbusinessID;
    /**
     * 密钥
     */
    private String kdniaoAppKey;

    /**
     * 快递100 授权码，请申请企业版获取
     */
    private String kuaidi100Customer;
    /**
     * 快递100 Key
     */
    private String kuaidi100Key;

    /**
     * 顺丰顾客编码
     */
    String clientCode;

    /**
     * 顺丰校验码
     */
    String checkWord;

    /**
     * 顺丰请求地址
     */
    String callUrl;

    /**
     * 顺丰打印模板
     */
    String templateCode;

    /**
     * 顺丰月结号
     */
    String monthlyCardNo;
}
