package cn.lili.modules.goods.mapper;

import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsSkuDTO;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Insert;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

import java.util.List;

/**
 * 规格项数据处理层
 *
 * @author pikachu
 * @since 2020-02-18 15:18:56
 */
public interface GoodsSkuMapper extends BaseMapper<GoodsSku> {

    /**
     * 根据商品id获取全部skuId的集合
     *
     * @param goodsId goodsId
     * @return 全部skuId的集合
     */
    @Select("SELECT id FROM li_goods_sku WHERE goods_id = #{goodsId}")
    List<String> getGoodsSkuIdByGoodsId(String goodsId);


    @Insert("replace into li_goods_sku (\n" +
            "\tid,\n" +
            "\tgoods_id,\n" +
            "\tspecs,\n" +
            "\tsimple_specs,\n" +
            "\tfreight_template_id,\n" +
            "\tgoods_name,\n" +
            "\tsn,\n" +
            "\tbrand_id,\n" +
            "\tcategory_path,\n" +
            "\tgoods_unit,\n" +
            "\tselling_point,\n" +
            "\tweight,\n" +
            "\tmarket_enable,\n" +
            "\tintro,\n" +
            "\tprice,\n" +
            "\tcost,\n" +
            "\tquantity,\n" +
            "\tgrade,\n" +
            "\tthumbnail,\n" +
            "\tsmall,\n" +
            "\tstore_category_path,\n" +
            "\tstore_id,\n" +
            "\tstore_name,\n" +
            "\tauth_flag,\n" +
            "\tself_operated,\n" +
            "\tmobile_intro,\n" +
            "\trecommend,\n" +
            "\tsales_model,\n" +
            "\tgoods_type,\n" +
            "\tcreate_by,\n" +
            "\tcreate_time,\n" +
            "\tupdate_time,\n" +
            "\tdelete_flag \n" +
            ")\n" +
            " VALUES\n" +
            "(\n" +
            "\t#{goodsSku.id},\n" +
            "\t#{goodsSku.goodsId},\n" +
            "\t#{goodsSku.specs},\n" +
            "\t#{goodsSku.simpleSpecs},\n" +
            "\t#{goodsSku.freightTemplateId},\n" +
            "\t#{goodsSku.goodsName},\n" +
            "\t#{goodsSku.sn},\n" +
            "\t#{goodsSku.brandId},\n" +
            "\t#{goodsSku.categoryPath},\n" +
            "\t#{goodsSku.goodsUnit},\n" +
            "\t#{goodsSku.sellingPoint},\n" +
            "\t#{goodsSku.weight},\n" +
            "\t#{goodsSku.marketEnable},\n" +
            "\t#{goodsSku.intro},\n" +
            "\t#{goodsSku.price},\n" +
            "\t#{goodsSku.cost},\n" +
            "\t#{goodsSku.quantity},\n" +
            "\t#{goodsSku.grade},\n" +
            "\t#{goodsSku.thumbnail},\n" +
            "\t#{goodsSku.small},\n" +
            "\t#{goodsSku.storeCategoryPath},\n" +
            "\t#{goodsSku.storeId},\n" +
            "\t#{goodsSku.storeName},\n" +
            "\t#{goodsSku.authFlag},\n" +
            "\t#{goodsSku.selfOperated},\n" +
            "\t#{goodsSku.mobileIntro},\n" +
            "\t#{goodsSku.recommend},\n" +
            "\t#{goodsSku.salesModel},\n" +
            "\t#{goodsSku.goodsType},\n" +
            "\t#{goodsSku.createBy},\n" +
            "\t#{goodsSku.createTime},\n" +
            "\t#{goodsSku.updateTime},\n" +
            "\t#{goodsSku.deleteFlag}\n" +
            ")")
    int replaceGoodsSku(@Param("goodsSku") GoodsSku goodsSku);


    /**
     * 分页查询商品skuDTO
     *
     * @param page         分页
     * @param queryWrapper 查询条件
     * @return 售后VO分页
     */
    @Select("SELECT *,g.params as params FROM li_goods_sku gs inner join li_goods g on gs.goods_id = g.id ${ew.customSqlSegment}")
    IPage<GoodsSkuDTO> queryByParams(IPage<GoodsSkuDTO> page, @Param(Constants.WRAPPER) Wrapper<GoodsSkuDTO> queryWrapper);

}