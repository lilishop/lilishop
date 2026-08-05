package cn.lili.modules.message.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;


/**
 * 短链接/暂时只用于小程序二维码业务
 *
 * @author Chopper
 */
@Data
@EqualsAndHashCode(callSuper = false)
@TableName("li_short_link")
@Schema(description = "短链接/暂时只用于小程序二维码业务")
public class ShortLink extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @Schema(description = "原始参数")
    private String originalParams;


}
