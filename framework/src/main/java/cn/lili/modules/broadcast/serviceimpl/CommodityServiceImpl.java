package cn.lili.modules.broadcast.serviceimpl;

import cn.hutool.core.convert.Convert;
import cn.hutool.json.JSONObject;
import cn.lili.common.security.context.UserContext;
import cn.lili.modules.broadcast.entity.dos.Commodity;
import cn.lili.modules.broadcast.mapper.CommodityMapper;
import cn.lili.modules.broadcast.service.CommodityService;
import cn.lili.modules.broadcast.util.WechatLivePlayerUtil;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;

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

    @Override
    public boolean addCommodity(List<Commodity> commodityList) {
        for (Commodity commodity:commodityList) {
            JSONObject json = wechatLivePlayerUtil.addGoods(commodity);
            commodity.setLiveGoodsId(Convert.toInt(json.getStr("goodsId")));
            commodity.setAuditId(json.getStr("auditId"));
            commodity.setStoreId(UserContext.getCurrentUser().getStoreId());
        }
        return this.saveBatch(commodityList);
    }

    @Override
    public boolean deleteCommodity(String goodsId) {
        JSONObject json = wechatLivePlayerUtil.deleteGoods(goodsId);
        if (json.getStr("errcode").equals("0")) {
            return this.remove(this.lambdaQuery().eq(Commodity::getLiveGoodsId, goodsId));
        }
        return false;
    }

    @Override
    public void getGoodsWareHouse() {
        //查询审核中的商品
        List<String> goodsIdList = this.baseMapper.getAuditCommodity(UserContext.getCurrentUser().getStoreId());
        //同步状态
        JSONObject json = wechatLivePlayerUtil.getGoodsWareHouse(goodsIdList);
        //修改状态
        List<Map<String, String>> list = (List) json.get("goods");
        for (Map<String, String> map : list) {
            //修改审核状态
            this.update(this.lambdaUpdate()
                    .eq(Commodity::getLiveGoodsId, map.get("goods_id"))
                    .set(Commodity::getAuditStatus, map.get("audit_status")));
        }
    }


}
