package cn.lili.modules.promotion.entity.vos.kanjia;

import cn.lili.modules.promotion.entity.dos.KanjiaActivity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 砍价活动参与实体类
 *
 * @author qiuqiu
 * @date 2020-7-1 10:44 上午
 */
@Data
@ApiModel(value = "砍价活动VO")
public class KanjiaActivityVO extends KanjiaActivity {

    @ApiModelProperty(value = "是否可以砍价")
    private Boolean help;

    @ApiModelProperty(value = "是否已发起砍价")
    private Boolean launch;

    @ApiModelProperty(value = "是否可购买")
    private Boolean pass;

    public KanjiaActivityVO() {
        this.setHelp(false);
        this.setLaunch(false);
        this.setPass(false);
    }

}