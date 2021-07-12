package cn.lili.modules.broadcast.serviceimpl;

import cn.hutool.core.convert.Convert;
import cn.hutool.json.JSONArray;
import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.utils.PageUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.broadcast.entity.dos.Commodity;
import cn.lili.modules.broadcast.entity.dto.CommodityDTO;
import cn.lili.modules.broadcast.entity.vos.CommodityVO;
import cn.lili.modules.broadcast.mapper.CommodityMapper;
import cn.lili.modules.broadcast.service.CommodityService;
import cn.lili.modules.broadcast.util.WechatLivePlayerUtil;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.service.GoodsSkuService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * 直播商品业务层实现
 *
 * @author Bulbasaur
 * @date: 2021/5/17 11:16 上午
 */
@Service
public class CommodityServiceImpl extends ServiceImpl<CommodityMapper, Commodity> implements CommodityService {

    @Autowired
    private WechatLivePlayerUtil wechatLivePlayerUtil;
    @Autowired
    private GoodsSkuService goodsSkuService;

    @Override
    public boolean addCommodity(List<Commodity> commodityList) {
        for (Commodity commodity : commodityList) {
            //检测直播商品
            checkCommodity(commodity);
            //添加直播商品
            JSONObject json = wechatLivePlayerUtil.addGoods(commodity);
            if(!"0".equals(json.getStr("errcode"))){
                log.error(json.getStr("errmsg"));
                throw new ServiceException(ResultCode.COMMODITY_ERROR);
            }
            commodity.setLiveGoodsId(Convert.toInt(json.getStr("goodsId")));
            commodity.setAuditId(json.getStr("auditId"));
            commodity.setStoreId(UserContext.getCurrentUser().getStoreId());
            //默认为待审核状态
            commodity.setAuditStatus("0");
            this.save(commodity);
        }
        return true;
    }

    private void checkCommodity(Commodity commodity) {
        //商品是否审核通过
        GoodsSku goodsSku = goodsSkuService.getById(commodity.getSkuId());
        if (!goodsSku.getIsAuth().equals(GoodsAuthEnum.PASS.name())) {
            throw new ServiceException(goodsSku.getGoodsName() + " 未审核通过，不能添加直播商品");
        }
        //是否已添加规格商品
        if (this.count(new LambdaQueryWrapper<Commodity>().eq(Commodity::getSkuId, commodity.getSkuId())) > 0) {
            throw new ServiceException(goodsSku.getGoodsName() + " 已添加规格商品，无法重复增加");
        }
    }

    @Override
    public boolean deleteCommodity(String goodsId) {
        JSONObject json = wechatLivePlayerUtil.deleteGoods(goodsId);
        if ("0".equals(json.getStr("errcode"))) {
            return this.remove(new LambdaQueryWrapper<Commodity>().eq(Commodity::getLiveGoodsId, goodsId));
        }
        return false;
    }

    @Override
    public void getGoodsWareHouse() {
        //查询审核中的商品
        List<String> goodsIdList = this.baseMapper.getAuditCommodity();
        if (goodsIdList.size() > 0) {
            //同步状态
            JSONObject json = wechatLivePlayerUtil.getGoodsWareHouse(goodsIdList);
            //修改状态
            List<CommodityDTO> commodityDTOList = JSONUtil.toList((JSONArray) json.get("goods"), CommodityDTO.class);
            for (CommodityDTO commodityDTO : commodityDTOList) {
                //修改审核状态
                this.update(new LambdaUpdateWrapper<Commodity>()
                        .eq(Commodity::getLiveGoodsId, commodityDTO.getGoods_id())
                        .set(Commodity::getAuditStatus, commodityDTO.getAudit_status()));
            }
        }
    }

    @Override
    public IPage<CommodityVO> commodityList(PageVO pageVO, String name, String auditStatus) {
        return this.baseMapper.commodityVOList(PageUtil.initPage(pageVO),
                new QueryWrapper<CommodityVO>().like(name != null, "c.name", name)
                        .eq(auditStatus != null, "c.audit_status", auditStatus)
                        .eq(UserContext.getCurrentUser().getRole().equals(UserEnums.STORE), "c.store_id", UserContext.getCurrentUser().getStoreId())
                        .orderByDesc("create_time"));
    }
}
