package cn.lili.modules.logistics.plugin.kuaidi100;

import cn.hutool.core.text.CharSequenceUtil;
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
import com.kuaidi100.sdk.response.labelV2.OrderResult;
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
    public Map<String, Object> labelOrder(LabelOrderDTO labelOrderDTO) {
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



            OrderReq orderReq = new OrderReq();
            //打印类型，NON：只下单不打印（默认）； IMAGE:生成图片短链；HTML:生成html短链； CLOUD:使用快递100云打印机打印，使用CLOUD时siid必填
            orderReq.setPrintType(PrintType.HTML);
            //电子面单客户账户或月结账号，需贵司向当地快递公司网点申请（参考电子面单申请指南）； 是否必填该属性，请查看参数字典
            orderReq.setPartnerId(storeLogistics.getCustomerName());
            //电子面单密码，需贵司向当地快递公司网点申请； 是否必填该属性，请查看参数字典
            if(CharSequenceUtil.isNotEmpty(storeLogistics.getCustomerPwd())){
                orderReq.setPartnerKey(storeLogistics.getCustomerPwd());
            }

            //电子面单密钥，需贵司向当地快递公司网点申请； 是否必填该属性，请查看参数字典
            if(CharSequenceUtil.isNotEmpty(storeLogistics.getMonthCode())) {
                orderReq.setPartnerSecret(storeLogistics.getMonthCode());
            }
            //电子面单客户账户名称，需贵司向当地快递公司网点申请； 是否必填该属性，请查看参数字典
            if(CharSequenceUtil.isNotEmpty(storeLogistics.getPartnerName())) {
                orderReq.setPartnerName(storeLogistics.getPartnerName());
            }
//            orderReq.setNet();
            //	电子面单承载编号，需贵司向当地快递公司网点申请； 是否必填该属性，请查看参数字典
            if(CharSequenceUtil.isNotEmpty(storeLogistics.getSendSite())) {
                orderReq.setCode(storeLogistics.getSendSite());
            }
            //电子面单承载快递员名，需贵司向当地快递公司网点申请； 是否必填该属性，请查看参数字典
            if(CharSequenceUtil.isNotEmpty(storeLogistics.getSendStaff())) {
                orderReq.setCheckMan(storeLogistics.getSendStaff());
            }

            //快递公司的编码，一律用小写字母，请查看参数字典
            orderReq.setKuaidicom(logistics.getCode());
            //收件人信息
            ManInfo manInfo=new ManInfo();
            //收件人姓名
            manInfo.setName(order.getConsigneeName());
            //收件人的手机号，手机号和电话号二者其一必填
            manInfo.setMobile(order.getConsigneeMobile());
            //收件人的电话号，手机号和电话号二者其一必填
//            manInfo.setTel("");
            //收件人所在完整地址，如广东深圳市南山区科技南十二路金蝶软件园B10
            manInfo.setPrintAddr(consigneeAddress[0]+consigneeAddress[1]+consigneeAddress[2]+consigneeAddress[3]+order.getConsigneeDetail());
            orderReq.setRecMan(manInfo);
            ManInfo sendMan=new ManInfo();
            //	寄件人信息
            sendMan.setName(storeDeliverGoodsAddressDTO.getSalesConsignorName());
            //	寄件人的手机号，手机号和电话号二者其一必填
            sendMan.setMobile(storeDeliverGoodsAddressDTO.getSalesConsignorMobile());
            //寄件人的电话号，手机号和电话号二者其一必填
//            sendMan.setTel("");
            //寄件人所在的完整地址，如广东深圳市南山区科技南十二路金蝶软件园B10
            sendMan.setPrintAddr(consignorAddress[0]+consignorAddress[1]+consignorAddress[2]+consignorAddress[3]+storeDeliverGoodsAddressDTO.getSalesConsignorDetail());
            //寄件人所在公司名称
//            sendMan.setCompany("");
            orderReq.setSendMan(sendMan);
            //物品名称,例：文件
            String goodsName="";
            for (OrderItem orderItem : orderItems) {
                goodsName+=orderItem.getGoodsName() + "',";
            }

            orderReq.setCargo(goodsName);
            //	包裹总数量。
            orderReq.setCount(orderItems.size());
            //打印设备，通过打印机输出的设备码进行获取，printType为CLOUD时必填
//            orderReq.setSiid("");

            // orderReq.setSiid(siid);
            //orderReq.setTempId("60f6c17c7c223700131d8bc3");

            String param = new Gson().toJson(orderReq);
            String t = System.currentTimeMillis() + "";

            PrintReq printReq = new PrintReq();
            printReq.setT(t);
            printReq.setKey(logisticsSetting.getKuaidi100Key());
            printReq.setSign(SignUtils.printSign(param, t, logisticsSetting.getKuaidi100Key(), logisticsSetting.getKuaidi100Customer()));
            printReq.setMethod(ApiInfoConstant.NEW_TEMPLATE_URL);
            printReq.setParam(param);

            IBaseClient baseClient = new LabelV2();
            HttpResult result = baseClient.execute(printReq);
            System.out.println(result.getBody());



            OrderResult orderResult = new Gson().fromJson(result.getBody(), OrderResult.class);
            log.info("电子面单响应：{}", orderResult);
            System.out.println("快递单号："+orderResult.getKdComOrderNum());
            System.out.println("面单短链："+orderResult.getLabel());

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
