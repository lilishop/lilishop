package cn.lili.modules.distribution.entity.dto;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.util.Objects;

/**
 * 分销员商品查询条件
 *
 * @author pikachu
 * @since 2020-03-14 23:04:56
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class DistributionGoodsSearchParams extends PageVO {

    @Schema(description = "商品ID")
    private String goodsId;

    @Schema(description = "商品名称")
    private String goodsName;

    @Schema(description = "店铺名称")
    private String storeName;

    @Schema(description = "平台类目ID")
    private String categoryId;

    @Schema(description = "品牌ID")
    private String brandId;

    @Schema(description = "店铺分类")
    private String storeCategoryPath;

    @Schema(description = "商品类型")
    private String goodsType;

    @Schema(description = "审核状态")
    private String authStatus;

    @Schema(description = "排序字段：COMMISSION LATEST SALES PRICE")
    private String sortField;

    @Schema(description = "排序方向：ASC DESC")
    private String sortOrder;

    public <T> QueryWrapper<T> queryWrapper() {
        QueryWrapper<T> queryWrapper = this.distributionQueryWrapper();
        queryWrapper.eq(CharSequenceUtil.isNotEmpty(goodsId), "goods_id", goodsId);
        queryWrapper.eq(CharSequenceUtil.isNotEmpty(goodsName), "goods_name", goodsId);
        return queryWrapper;
    }

    public <T> QueryWrapper<T> storeQueryWrapper() {
        QueryWrapper<T> queryWrapper = this.distributionQueryWrapper();
        queryWrapper.eq("dg.store_id", Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId());
        return queryWrapper;
    }

    public <T> QueryWrapper<T> distributionQueryWrapper() {
        QueryWrapper<T> queryWrapper = new QueryWrapper<>();
        queryWrapper.like(CharSequenceUtil.isNotEmpty(goodsName), "dg.goods_name", goodsName);
        queryWrapper.eq(CharSequenceUtil.isNotEmpty(storeName), "dg.store_name", storeName);
        queryWrapper.like(CharSequenceUtil.isNotEmpty(categoryId), "gs.category_path", categoryId);
        queryWrapper.eq(CharSequenceUtil.isNotEmpty(brandId), "gs.brand_id", brandId);
        queryWrapper.like(CharSequenceUtil.isNotEmpty(storeCategoryPath), "gs.store_category_path", storeCategoryPath);
        queryWrapper.eq(CharSequenceUtil.isNotEmpty(goodsType), "gs.goods_type", goodsType);
        queryWrapper.eq(CharSequenceUtil.isNotEmpty(authStatus), "dg.auth_status", authStatus);
        return queryWrapper;
    }

}
