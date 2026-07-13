package cn.lili.event;


import cn.lili.modules.store.entity.dos.Store;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
public interface StoreSettingChangeEvent {

    /**
     * 店铺信息更改消息
     *
     * @param store 店铺信息
     */
    void storeSettingChange(Store store);
}
