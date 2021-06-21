package cn.lili.modules.broadcast.entity.enums;


/**
 * 直播间状态
 * @author Bulbasaur
 * @date: 2021/5/31 10:32 上午
 *
 */
public enum StudioStatusEnum {

    NEW("新建"), START("开始"), END("结束");

    private final String clientName;

    StudioStatusEnum(String des) {
        this.clientName = des;
    }

    public String clientName() {
        return this.clientName;
    }

    public String value() {
        return this.name();
    }
}
