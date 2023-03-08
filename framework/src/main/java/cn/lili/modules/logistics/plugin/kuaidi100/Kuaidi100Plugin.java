package cn.lili.modules.logistics.plugin.kuaidi100;

import cn.lili.modules.logistics.LogisticsPlugin;
import cn.lili.modules.logistics.entity.dto.LabelOrderDTO;
import cn.lili.modules.logistics.entity.enums.LogisticsEnum;
import cn.lili.modules.logistics.plugin.kuaidi100.utils.Kuaidi100SignUtils;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.vo.OrderDetailVO;
import cn.lili.modules.store.entity.dos.StoreLogistics;
import cn.lili.modules.store.entity.dto.StoreDeliverGoodsAddressDTO;
import cn.lili.modules.system.entity.dos.Logistics;
import cn.lili.modules.system.entity.dto.LogisticsSetting;
import cn.lili.modules.system.entity.vo.Traces;
import com.google.gson.Gson;
import com.kuaidi100.sdk.api.LabelV2;
import com.kuaidi100.sdk.api.QueryTrack;
import com.kuaidi100.sdk.api.QueryTrackMap;
import com.kuaidi100.sdk.contant.ApiInfoConstant;
import com.kuaidi100.sdk.contant.PrintType;
import com.kuaidi100.sdk.core.IBaseClient;
import com.kuaidi100.sdk.pojo.HttpResult;
import com.kuaidi100.sdk.request.ManInfo;
import com.kuaidi100.sdk.request.PrintReq;
import com.kuaidi100.sdk.request.QueryTrackParam;
import com.kuaidi100.sdk.request.QueryTrackReq;
import com.kuaidi100.sdk.request.labelV2.OrderReq;
import com.kuaidi100.sdk.response.QueryTrackData;
import com.kuaidi100.sdk.response.QueryTrackMapResp;
import com.kuaidi100.sdk.response.QueryTrackResp;
import com.kuaidi100.sdk.response.samecity.OrderResp;
import com.kuaidi100.sdk.utils.SignUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 快递100插件
 *
 * @author Bulbasaur
 */
@Slf4j
public class Kuaidi100Plugin implements LogisticsPlugin {


    @Autowired
    private LogisticsSetting logisticsSetting;

    public Kuaidi100Plugin(LogisticsSetting logisticsSetting) {
        this.logisticsSetting = logisticsSetting;
    }

    @Override
    public LogisticsEnum pluginName() {
        return LogisticsEnum.KUAIDI100;
    }

    @Override
    public Traces pollQuery(Logistics logistics, String expNo, String phone) {
        try {
            QueryTrackReq queryTrackReq = new QueryTrackReq();
            QueryTrackParam queryTrackParam = new QueryTrackParam();
            queryTrackParam.setCom(logistics.getCode());
            queryTrackParam.setNum(expNo);
            queryTrackParam.setPhone(phone);
            String param = new Gson().toJson(queryTrackParam);

            queryTrackReq.setParam(param);
            queryTrackReq.setCustomer(logisticsSetting.getKuaidi100Customer());
            queryTrackReq.setSign(Kuaidi100SignUtils.querySign(param, logisticsSetting.getKuaidi100Key(), logisticsSetting.getKuaidi100Customer()));

            IBaseClient baseClient = new QueryTrack();
            HttpResult httpResult = baseClient.execute(queryTrackReq);
            QueryTrackResp queryTrackResp = new Gson().fromJson(httpResult.getBody(), QueryTrackResp.class);
            log.info(String.valueOf(queryTrackResp.getData()));

            List<Map> traces = new ArrayList<>();
            for (QueryTrackData queryTrackData : queryTrackResp.getData()) {
                Map map = new HashMap<String, String>();
                map.put("AcceptTime", queryTrackData.getTime());
                map.put("AcceptStation", queryTrackData.getContext());
                map.put("Remark", null);
                traces.add(map);
            }
            return new Traces(logistics.getName(), expNo, traces);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    @Override
    public Traces pollMapTrack(Logistics logistics, String expNo, String phone, String from, String to) {
        try {
            QueryTrackReq queryTrackReq = new QueryTrackReq();
            QueryTrackParam queryTrackParam = new QueryTrackParam();
            queryTrackParam.setCom(logistics.getCode());
            queryTrackParam.setNum(expNo);
            queryTrackParam.setPhone(phone);
            queryTrackParam.setFrom(from);
            queryTrackParam.setTo(to);
            queryTrackParam.setResultv2("5");
            String param = new Gson().toJson(queryTrackParam);

            queryTrackReq.setParam(param);
            queryTrackReq.setCustomer(logisticsSetting.getKuaidi100Customer());
            queryTrackReq.setSign(SignUtils.querySign(param, logisticsSetting.getKuaidi100Key(), logisticsSetting.getKuaidi100Customer()));

            IBaseClient baseClient = new QueryTrackMap();
            HttpResult result = baseClient.execute(queryTrackReq);

            QueryTrackMapResp queryTrackMapResp = new Gson().fromJson(result.getBody(), QueryTrackMapResp.class);
            System.out.println(queryTrackMapResp);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    @Override
    public Map<String,Object> labelOrder(LabelOrderDTO labelOrderDTO) {
        try {
            //订单
            Order order = labelOrderDTO.getOrder();
            //订单货物
            List<OrderItem> orderItems = labelOrderDTO.getOrderItems();
            //获取对应物流
            Logistics logistics = labelOrderDTO.getLogistics();
            //收件人地址
            String[] consigneeAddress = order.getConsigneeAddressPath().split(",");
            //获取店家信息
            StoreDeliverGoodsAddressDTO storeDeliverGoodsAddressDTO = labelOrderDTO.getStoreDeliverGoodsAddressDTO();
            //发件人地址
            String[] consignorAddress = storeDeliverGoodsAddressDTO.getSalesConsignorAddressPath().split(",");
            //店铺-物流公司设置
            StoreLogistics storeLogistics = labelOrderDTO.getStoreLogistics();


            ManInfo recManInfo = new ManInfo();
            recManInfo.setName(order.getConsigneeName());
            recManInfo.setMobile(order.getConsigneeMobile());
            recManInfo.setPrintAddr(consigneeAddress[0] + consigneeAddress[1] + consigneeAddress[2] + consigneeAddress[3] + order.getConsigneeDetail());

            ManInfo sendManInfo = new ManInfo();
            sendManInfo.setName(storeDeliverGoodsAddressDTO.getSalesConsignorName());
            sendManInfo.setMobile(storeDeliverGoodsAddressDTO.getSalesConsignorMobile());
            sendManInfo.setPrintAddr(consignorAddress[0] + consignorAddress[1] + consignorAddress[2] + consignorAddress[3] + storeDeliverGoodsAddressDTO.getSalesConsignorDetail());

            OrderReq orderReq = new OrderReq();
            orderReq.setKuaidicom(logistics.getCode());
            orderReq.setCount(1);
            // orderReq.setSiid(siid);
            //orderReq.setTempId("60f6c17c7c223700131d8bc3");
            orderReq.setSendMan(sendManInfo);
            orderReq.setRecMan(recManInfo);

            orderReq.setPrintType(PrintType.CLOUD);

            String param = new Gson().toJson(orderReq);
            String t = System.currentTimeMillis() + "";

            PrintReq printReq = new PrintReq();
            printReq.setT(t);
            printReq.setKey(logisticsSetting.getKuaidi100Key());
            printReq.setSign(SignUtils.printSign(param, t, logisticsSetting.getKuaidi100Key(), logisticsSetting.getKuaidi100Customer()));
            printReq.setMethod(ApiInfoConstant.ORDER);
            printReq.setParam(param);

            IBaseClient baseClient = new LabelV2();
            HttpResult result = baseClient.execute(printReq);
            System.out.println(result.getBody());
            QueryTrackMapResp queryTrackMapResp = new Gson().fromJson(result.getBody(), QueryTrackMapResp.class);
            OrderResp orderResp = new Gson().fromJson(result.getBody(), OrderResp.class);

        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    @Override
    public String createOrder(OrderDetailVO orderDetailVO) {
        return null;
    }


}
