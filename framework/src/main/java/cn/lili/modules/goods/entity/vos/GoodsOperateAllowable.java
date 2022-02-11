package cn.lili.modules.goods.entity.vos;

import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * 商品操作允许的范围
 *
 * @author Bulbasaur
 * @since 2020-02-26 23:24:13
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class GoodsOperateAllowable implements Serializable {

    /**
     * 上下架状态
     *
     * @see GoodsStatusEnum
     */
    private String marketEnable;

    /**
     * 删除状态 true 已删除 false 未删除
     */
    private Boolean deleteFlag;

    /**
     * 是否允许下架
     */
    private Boolean allowDown;
    /**
     * 是否允许放入回收站
     */
    private Boolean allowDelete;
    /**
     * 是否允许回收站的商品还原
     */
    private Boolean allowReduction;
    /**
     * 是否允许回收站的商品彻底删除
     */
    private Boolean allowClear;
    /**
     * 是否允许上架
     */
    private Boolean allowUpper;

    /**
     * 构造函数
     *
     * @param marketEnable
     * @param deleteFlag
     */
    public GoodsOperateAllowable(String marketEnable, Boolean deleteFlag) {
        this.marketEnable = marketEnable;
        this.deleteFlag = deleteFlag;
    }


    public Boolean getAllowDown() {
        //上架状态 不在回收站的商品可以下架
        return marketEnable == GoodsStatusEnum.UPPER.name() && deleteFlag == false;
    }

    public Boolean getAllowReduction() {
        //下架状态 在回收站的商品可以还原
        return marketEnable == GoodsStatusEnum.DOWN.name() && deleteFlag == true;
    }

    public Boolean getAllowClear() {
        //下架状态 在回收站的商品可以彻底删除
        return marketEnable == GoodsStatusEnum.DOWN.name() && deleteFlag == true;
    }

    public Boolean getAllowUpper() {
        //下架状态 未删除的商品可以上架
        return marketEnable == GoodsStatusEnum.DOWN.name() && deleteFlag == false;
    }

    public Boolean getAllowDelete() {
        //下架状态 未删除的商品可以删除
        return marketEnable == GoodsStatusEnum.DOWN.name() && deleteFlag == false;
    }

}
