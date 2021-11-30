package cn.lili.modules.order.aftersale.entity.enums;

/**
 * 交易投诉状态
 *
 * @author paulG
 * @since 2020/12/4
 **/
public enum ComplaintStatusEnum {

    /**
     * 新投诉
     */
    NEW("新投诉"),
    /**
     * 已撤销
     */
    CANCEL("已撤销"),
    /**
     * 待申诉
     */
    WAIT_APPEAL("待申诉"),
    /**
     * 对话中
     */
    COMMUNICATION("对话中"),
    /**
     * 等待仲裁
     */
    WAIT_ARBITRATION("等待仲裁"),
    /**
     * 已完成
     */
    COMPLETE("已完成");

    private final String description;

    ComplaintStatusEnum(String description) {
        this.description = description;
    }

    public String description() {
        return this.description;
    }


}
