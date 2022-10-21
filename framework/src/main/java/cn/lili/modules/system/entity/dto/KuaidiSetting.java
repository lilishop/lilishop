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
public class KuaidiSetting implements Serializable {
    private static final long serialVersionUID = 3520379500723173689L;
    /**
     * 企业id
     */
    private String ebusinessID;
    /**
     * 密钥
     */
    private String appKey;
    /**
     * api地址
     */
    private String reqURL;

    /**
     * 电子面单api地址
     */
    private String sheetReqURL;
}
