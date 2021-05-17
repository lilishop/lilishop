package cn.lili.modules.broadcast.entity.dto;

/**
 * @author liushuai(liushuai711 @ gmail.com)
 * @version v4.1
 * @Description:
 * @since 2021/5/17 11:34 上午
 */
public enum CommodityAuditStatusEnum {

    /**
     * 未审核
     */
    WAIT_AUDIT("0"),
    /**
     * 审核中
     */
    AUDITING("1"),
    /**
     * 审核通过
     */
    AUDIT_PASS("2"),
    /**
     * 审核失败
     */
    AUDIT_REJECTION("3");

    private final String status;

    CommodityAuditStatusEnum(String status) {
        this.status = status;
    }

    public String getStatus() {
        return status;
    }

}
