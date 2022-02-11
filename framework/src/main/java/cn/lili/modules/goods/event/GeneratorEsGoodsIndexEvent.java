package cn.lili.modules.goods.event;

import lombok.Data;
import org.springframework.context.ApplicationEvent;

/**
 * @author paulG
 * @since 2022/1/19
 **/
@Data
public class GeneratorEsGoodsIndexEvent extends ApplicationEvent {

    private String goodsId;

    public GeneratorEsGoodsIndexEvent(Object source, String goodsId) {
        super(source);
        this.goodsId = goodsId;
    }
}
