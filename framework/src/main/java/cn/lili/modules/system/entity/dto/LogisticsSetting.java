package cn.lili.modules.system.entity.dto;

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
}
