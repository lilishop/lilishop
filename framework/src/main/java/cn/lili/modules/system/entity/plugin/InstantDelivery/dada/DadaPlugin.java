package cn.lili.modules.system.entity.plugin.InstantDelivery.dada;

import cn.hutool.json.JSONArray;
import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import cn.lili.common.utils.DateUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.modules.member.entity.dos.MemberAddress;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.store.entity.enums.StoreStatusEnum;
import cn.lili.modules.store.entity.vos.StoreDetailVO;
import cn.lili.modules.store.service.StoreDetailService;
import cn.lili.modules.system.entity.dos.InstantDeliveryLog;
import cn.lili.modules.system.entity.enums.InstantDeliveryUrl;
import cn.lili.modules.system.entity.plugin.ConfigItem;
import cn.lili.modules.system.entity.plugin.InstantDelivery.InstantDeliveryPlugin;
import cn.lili.modules.system.entity.plugin.InstantDelivery.dada.vo.DdOrderBackVO;
import cn.lili.modules.system.entity.vo.CityResult;
import cn.lili.modules.system.entity.vo.InstantDeliveryResultVO;
import cn.lili.modules.system.service.InstantDeliveryLogService;
import cn.lili.modules.system.utils.HttpUtils;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 达达同城配送
 *
 * @author pikachu
 * @version v4.0
 * @Description:
 * @since 2020/12/01 15:58
 */
@Slf4j
@Component("ddPlugin")
public class DadaPlugin implements InstantDeliveryPlugin {

    @Autowired
    private StoreDetailService storeDetailService;
    @Autowired
    private InstantDeliveryLogService instantDeliveryLogService;

    @Override
    public String getPluginId() {
        return "ddPlugin";
    }

    @Override
    public String getPluginName() {
        return "达达";
    }

    @Override
    public Integer getOpen() {
        return 0;
    }

    @Override
    public List<ConfigItem> getDefaultConfigItem() {
        List<ConfigItem> list = new ArrayList();

        ConfigItem url = new ConfigItem();
        url.setType("text");
        url.setName("url");
        url.setText("调用地址");


        ConfigItem appKey = new ConfigItem();
        appKey.setType("text");
        appKey.setName("app_key");
        appKey.setText("app_key");

        ConfigItem appSecret = new ConfigItem();
        appSecret.setType("text");
        appSecret.setName("app_secret");
        appSecret.setText("app_secret");

        ConfigItem merchantsId = new ConfigItem();
        merchantsId.setType("text");
        merchantsId.setName("merchants_id");
        merchantsId.setText("商户Id");
        list.add(url);
        list.add(appKey);
        list.add(appSecret);
        list.add(merchantsId);
        return list;
    }

    @Override
    public InstantDeliveryResultVO addStore(StoreDetailVO storeDetailVO, Map config) {
        JSONArray jsonArray = new JSONArray();
        JSONObject jsonObject = new JSONObject();
//       //门店名称
//       jsonObject.put("station_name", storeDetailVO.getStoreName());
        //业务类型(食品小吃-1,饮料-2,鲜花-3,文印票务-8,便利店-9,水果生鲜-13,同城电商-19, 医药-20,蛋糕-21,酒品-24,小商品市场-25,服装-26,汽修零配-27,数码-28,小龙虾-29,火锅-51,其他-5)
        jsonObject.set("business", 19);
        //城市名称(如,上海)
//       jsonObject.put("city_name", storeDetailVO.getCompanyCity());
        //区域名称(如,浦东新区)
//       jsonObject.put("area_name", storeDetailVO.getCompanyCounty());
        //门店地址
        jsonObject.set("station_address", storeDetailVO.getCompanyAddress());
//       //门店经度
//       jsonObject.put("lng", storeDetailVO.getStoreLongitude());
//       //门店纬度
//       jsonObject.put("lat", storeDetailVO.getStoreLatitude());
        //联系人姓名
        jsonObject.set("contact_name", storeDetailVO.getLinkName());
        //联系人电话
        jsonObject.set("phone", storeDetailVO.getLinkPhone());
        //达达商家app账号
        jsonObject.set("username", storeDetailVO.getLinkPhone());
        //达达商家app密码
        jsonObject.set("password", "a" + storeDetailVO.getLinkPhone());
        jsonArray.add(jsonObject);
        //发送请求的url
        String url = StringUtils.toString(config.get("url"));
        Map<String, String> requestJson = getConfig(config, jsonArray.toString());
        String result = HttpUtils.doPostWithJson(url + InstantDeliveryUrl.DD_ADD_SHOP.getUrl(), requestJson);
        //组织返回参数
        InstantDeliveryResultVO instantDeliveryResultVO = JSONUtil.toBean(result, InstantDeliveryResultVO.class);
        Map<String, List> map = (Map<String, List>) instantDeliveryResultVO.getResult();
        List<Map<String, Object>> successList = map.get("successList");
        if (successList.size() > 0) {
            for (Map<String, Object> obj : successList) {
                //如果成功调用店铺修改 将门店编码写入数据库
                UpdateWrapper updateWrapper = new UpdateWrapper();
                updateWrapper.in("id", storeDetailVO.getStoreId());
                updateWrapper.set("dd_code", obj.get("originStoreId").toString());
                storeDetailService.update(updateWrapper);
            }
        }
        List<Map<String, Object>> errorList = map.get("failedList");
        if (errorList != null && errorList.size() > 0) {
            for (Map<String, Object> obj : errorList) {
                throw new RuntimeException(obj.get("msg").toString());
            }
        }
        return instantDeliveryResultVO;
    }

    @Override
    public InstantDeliveryResultVO editStore(StoreDetailVO storeDetailVO, Map config) {
        //如果达达code没有则不进行修改
        if (StringUtils.isEmpty(storeDetailVO.getDdCode())) {
            return null;
        }
        JSONObject jsonObject = new JSONObject();
        //门店编号
        jsonObject.set("origin_store_id", storeDetailVO.getDdCode());
        //门店名称
//       jsonObject.put("station_name", storeDetailVO.getStoreName());
        //业务类型(食品小吃-1,饮料-2,鲜花-3,文印票务-8,便利店-9,水果生鲜-13,同城电商-19, 医药-20,蛋糕-21,酒品-24,小商品市场-25,服装-26,汽修零配-27,数码-28,小龙虾-29,火锅-51,其他-5)
        jsonObject.set("business", 19);
        //城市名称(如,上海)
//       jsonObject.put("city_name", storeDetailVO.getCompanyCity());
        //区域名称(如,浦东新区)
//       jsonObject.put("area_name", storeDetailVO.getCompanyCounty());
        //门店地址
        jsonObject.set("station_address", storeDetailVO.getCompanyAddress());
//       //门店经度
//       jsonObject.put("lng", storeDetailVO.getStoreLongitude());
//       //门店纬度
//       jsonObject.put("lat", storeDetailVO.getStoreLatitude());
        //联系人姓名
        jsonObject.set("contact_name", storeDetailVO.getLinkName());
        //联系人电话
        jsonObject.set("phone", storeDetailVO.getLinkPhone());
        if (storeDetailVO.getStoreDisable().equals(StoreStatusEnum.OPEN.value())) {
            jsonObject.set("status", 1);
        } else {
            jsonObject.set("status", 0);
        }
        //发送请求的url
        String url = StringUtils.toString(config.get("url"));
        Map<String, String> requestJson = getConfig(config, jsonObject.toString());
        String result = HttpUtils.doPostWithJson(url + InstantDeliveryUrl.DD_UPDATE_SHOP.getUrl(), requestJson);
        //组织返回参数
        InstantDeliveryResultVO instantDeliveryResultVO = JSONUtil.toBean(result, InstantDeliveryResultVO.class);
        if ("fail".equals(instantDeliveryResultVO.getStatus())) {
            log.error("达达店铺信息修改失败",instantDeliveryResultVO.getMsg());
        }
        return instantDeliveryResultVO;
    }

    @Override
    public InstantDeliveryResultVO getOrderDetail(String orderSn, Map config) {
        JSONObject jsonObject = new JSONObject();
        jsonObject.set("order_id", orderSn);
        //发送请求的url
        String url = StringUtils.toString(config.get("url"));
        Map<String, String> requestJson = getConfig(config, jsonObject.toString());
        String result = HttpUtils.doPostWithJson(url + InstantDeliveryUrl.DD_QUERY_ORDER.getUrl(), requestJson);
        //组织返回参数
        InstantDeliveryResultVO instantDeliveryResultVO = JSONUtil.toBean(result, InstantDeliveryResultVO.class);
        return instantDeliveryResultVO;
    }

    @Override
    public InstantDeliveryResultVO orderConfirm(String orderSn, Map config) {
        JSONObject jsonObject = new JSONObject();
        jsonObject.set("order_id", orderSn);
        //发送请求的url
        String url = StringUtils.toString(config.get("url"));
        Map<String, String> requstJson = getConfig(config, jsonObject.toString());
        String result = HttpUtils.doPostWithJson(url + InstantDeliveryUrl.DD_CONFIRM_ORDER.getUrl(), requstJson);
        //组织返回参数
        InstantDeliveryResultVO instantDeliveryResultVO = JSONUtil.toBean(result, InstantDeliveryResultVO.class);
        return instantDeliveryResultVO;
    }

    @Override
    public InstantDeliveryResultVO orderCandle(String orderSn, String cancelReason, Map config) {
        JSONObject jsonObject = new JSONObject();
        jsonObject.set("order_id", orderSn);
        jsonObject.set("cancel_reason_id", this.cancelReasonId());
        jsonObject.set("cancel_reason", cancelReason);
        //发送请求的url
        String url = StringUtils.toString(config.get("url"));
        Map<String, String> requstJson = getConfig(config, jsonObject.toString());
        String result = HttpUtils.doPostWithJson(url + InstantDeliveryUrl.DD_CANDLE_ORDER.getUrl(), requstJson);
        //组织返回参数
        InstantDeliveryResultVO instantDeliveryResultVO = JSONUtil.toBean(result, InstantDeliveryResultVO.class);
        return instantDeliveryResultVO;
    }

    @Override
    public InstantDeliveryResultVO sendReOrder(Order order, StoreDetailVO storeDetailVO, MemberAddress memberAddress, Integer type, Map config) {
        JSONObject jsonObject = new JSONObject();
        //门店编号，门店创建后可在门店列表和单页查看
        jsonObject.set("store_no", storeDetailVO.getDdCode());
        //第三方订单ID
        jsonObject.set("origin_id", order.getSn());
        //订单所在城市的code http://newopen.imdada.cn/#/development/file/cityList?_k=l76a7s
        String city = memberAddress.getConsigneeAddressPath().substring(memberAddress.getConsigneeAddressPath().indexOf(",") + 1, memberAddress.getConsigneeAddressPath().indexOf(",", memberAddress.getConsigneeAddressPath().indexOf(",") + 1));
        jsonObject.set("city_code", this.getCityCode(city, config));
        //订单金额
        jsonObject.set("cargo_price", order.getFlowPrice());
        //是否需要垫付 1:是 0:否 (垫付订单金额，非运费)
        jsonObject.set("is_prepay", 0);
        //收货人姓名
        jsonObject.set("receiver_name", memberAddress.getName());
        //收货人地址
        jsonObject.set("receiver_address", memberAddress.getDetail());
        //收货人地址纬度（高德坐标系，若是其他地图经纬度需要转化成高德地图经纬度
        jsonObject.set("receiver_lat", memberAddress.getLat());
        //收货人地址经度（高德坐标系，若是其他地图经纬度需要转化成高德地图经纬度
        jsonObject.set("receiver_lng", memberAddress.getLat());
        //回调URL
        //jsonObject.put("callback", domainHelper.getCallback() + "/trade/delivery/order/call-back");
        //收货人手机号（手机号和座机号必填一项）
        jsonObject.set("receiver_phone", memberAddress.getMobile());
        //是否使用保价费（0：不使用保价，1：使用保价； 同时，请确保填写了订单金额（cargo_price））
        jsonObject.set("is_use_insurance", 0);
        //订单重量（单位：Kg）
        jsonObject.set("cargo_weight", order.getWeight());
        Map<String, String> requstJson = getConfig(config, jsonObject.toString());
        //发送请求的url
        String url = StringUtils.toString(config.get("url"));
        String result = null;
        if (type == 0) {
            result = HttpUtils.doPostWithJson(url + InstantDeliveryUrl.DD_ADD_ORDER.getUrl(), requstJson);
        } else {
            result = HttpUtils.doPostWithJson(url + InstantDeliveryUrl.DD_RE_ADD_ORDER.getUrl(), requstJson);
        }
        InstantDeliveryResultVO instantDeliveryResultVO = JSONUtil.toBean(result, InstantDeliveryResultVO.class);
        if ("fail".equals(instantDeliveryResultVO.getStatus())) {
            log.error("达达订单发送失败，订单号为",order.getSn() + "," + instantDeliveryResultVO.getMsg());
            //如果发送失败择等待一秒重新发送，如果失败择记录日志
            try {
                Thread.sleep(1000);
            } catch (Exception e) {
                log.error("达达订单发布失败",e);
            }
            result = HttpUtils.doPostWithJson(url + InstantDeliveryUrl.DD_RE_ADD_ORDER.getUrl(), requstJson);
            InstantDeliveryResultVO instantDeliveryResResultVO = JSONUtil.toBean(result, InstantDeliveryResultVO.class);
            if ("fail".equals(instantDeliveryResResultVO.getStatus())) {
                log.error("达达订单重试发送失败，订单号为" + order.getSn() + "," + instantDeliveryResultVO.getMsg());
            }
        }
        return instantDeliveryResultVO;
    }

    @Override
    public Integer cancelReasonId() {
        return 4;
    }

    /**
     * 达达配送统一参数整合
     *
     * @param config
     * @param json
     * @return
     */
    private Map<String, String> getConfig(Map config, String json) {
        //组织参数
        String appKey = StringUtils.toString(config.get("app_key"));
        String appSecret = StringUtils.toString(config.get("app_secret"));
        String merchantsId = StringUtils.toString(config.get("merchants_id"));
        //签名发送请求
        String mysing = appSecret + "app_key" + appKey + "body" + json + "formatjsonsource_id" + merchantsId + "timestamp" + DateUtil.getDateline() + "v1.0" + appSecret;
        String signature = StringUtils.md5(mysing).toUpperCase();
        Map<String, String> requstJson = new HashMap<>(8);
        requstJson.put("source_id", merchantsId);
        requstJson.put("app_key", appKey);
        requstJson.put("format", "json");
        requstJson.put("timestamp", StringUtils.toString(DateUtil.getDateline()));
        requstJson.put("signature", signature);
        requstJson.put("body", json);
        requstJson.put("v", "1.0");
        return requstJson;
    }

    /**
     * 获取城市code
     *
     * @param cityName 城市名称
     * @return 城市编码
     */
    private String getCityCode(String cityName, Map config) {
        //获取参数
        String url = StringUtils.toString(config.get("url"));
        JSONObject jsonObject = new JSONObject();
        Map<String, String> requstJson = getConfig(config, jsonObject.toString());
        //获取所有城市编码
        String result = HttpUtils.doPostWithJson(url + InstantDeliveryUrl.DD_CITY_CODE.getUrl(), requstJson);
        InstantDeliveryResultVO resultDO = JSONUtil.toBean(result, InstantDeliveryResultVO.class);
        //对数据进行格式化

        List<CityResult> list = JSONUtil.toList(JSONUtil.parseArray(resultDO.getResult()), CityResult.class);
        for (CityResult cityResult : list) {
            if (cityName.contains(cityResult.getCityName())) {
                return cityResult.getCityCode();
            }
        }
        //如果找不到默认哈尔滨
        return "0451";
    }

    @Override
    public void callBack(Object object) {
        //强制将obj转换成达达对应的参数对象
        DdOrderBackVO ddOrderBackVO = (DdOrderBackVO) object;
        //数据类型转换
        InstantDeliveryLog instantDeliveryLog = new InstantDeliveryLog(ddOrderBackVO);
        //保存数据
        instantDeliveryLogService.save(instantDeliveryLog);
    }

}
