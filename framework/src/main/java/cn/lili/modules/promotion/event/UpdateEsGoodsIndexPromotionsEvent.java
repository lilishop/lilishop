package cn.lili.modules.promotion.event;

import lombok.Data;
import org.springframework.context.ApplicationEvent;

/**
 * @author paulG
 * @since 2022/1/19
 **/
@Data
public class UpdateEsGoodsIndexPromotionsEvent extends ApplicationEvent {

    private String promotionsJsonStr;

    public UpdateEsGoodsIndexPromotionsEvent(Object source, String promotionsJsonStr) {
        super(source);
        this.promotionsJsonStr = promotionsJsonStr;
    }
}
