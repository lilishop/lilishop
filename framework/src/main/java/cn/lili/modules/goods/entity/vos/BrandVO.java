package cn.lili.modules.goods.entity.vos;

import cn.lili.modules.goods.entity.dos.Brand;
import com.fasterxml.jackson.databind.PropertyNamingStrategy;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import io.swagger.annotations.ApiModel;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 品牌VO
 *
 * @author pikachu
 * @since 2020-02-26 23:24:13
 */
@Data
public class BrandVO extends Brand {

    private static final long serialVersionUID = 3829199991161122317L;

}
