package cn.lili.modules.goods.event;

import lombok.Data;
import org.springframework.context.ApplicationEvent;

/**
 * @author paulG
 * @since 2022/1/19
 **/
@Data
public class GeneratorEsGoodsIndexEvent extends ApplicationEvent {

    private static final long serialVersionUID = -6206752641309458207L;

    private String id;

    private String tag;

    public GeneratorEsGoodsIndexEvent(Object source, String tag, String id) {
        super(source);
        this.tag = tag;
        this.id = id;
    }
}
