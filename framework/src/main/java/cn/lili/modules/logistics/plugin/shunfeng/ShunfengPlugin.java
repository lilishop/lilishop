package cn.lili.modules.logistics.plugin.shunfeng;

import cn.hutool.json.JSONArray;
import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.SpringContextUtil;
import cn.lili.modules.logistics.LogisticsPlugin;
import cn.lili.modules.logistics.entity.dto.LabelOrderDTO;
import cn.lili.modules.logistics.entity.enums.LogisticsEnum;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.vo.OrderDetailVO;
import cn.lili.modules.store.entity.dto.StoreDeliverGoodsAddressDTO;
import cn.lili.modules.store.service.StoreDetailService;
import cn.lili.modules.system.entity.dos.Logistics;
import cn.lili.modules.system.entity.dto.LogisticsSetting;
import cn.lili.modules.system.entity.vo.Traces;
import com.sf.csim.express.service.CallExpressServiceTools;
import com.sf.csim.express.service.HttpClientUtil;
import com.sf.csim.express.service.IServiceCodeStandard;
import com.sf.csim.express.service.code.ExpressServiceCodeEnum;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.io.UnsupportedEncodingException;
import java.util.*;

/**
 * 顺丰插件
 * @author admin
 */
@Slf4j
public class ShunfengPlugin implements LogisticsPlugin {

    /**
     * ExpressServiceCodeEnum 对应速运类-快递APIs
     * POSTServiceCodeEnum 对应速运类-驿站APIs
     * YJTServiceCodeEnum 对应解决方案-医寄通APIs
     * EPSServiceCodeEnum 对应解决方案-快递管家APIs
     * 详情见code目录下枚举类，客户可自行修改引用的该类
     **/
    private LogisticsSetting logisticsSetting;

    public ShunfengPlugin(){}

    public ShunfengPlugin(LogisticsSetting logisticsSetting) {
        this.logisticsSetting = logisticsSetting;
    }

    @Override
    public LogisticsEnum pluginName() {
        return LogisticsEnum.SHUNFENG;
    }

    /**
     * 文档地址：https://open.sf-express.com/Api/ApiDetails?level3=393&interName=%E4%B8%8B%E8%AE%A2%E5%8D%95%E6%8E%A5%E5%8F%A3-EXP_RECE_CREATE_ORDER
     *
     * @param orderDetailVO
     */
    public String createOrder(OrderDetailVO orderDetailVO) {
        StoreDetailService storeService = SpringContextUtil.getBean(StoreDetailService.class);
        StoreDeliverGoodsAddressDTO storeDeliverGoodsAddressDTO = storeService.getStoreDeliverGoodsAddressDto(orderDetailVO.getOrder().getStoreId());
        if(storeDeliverGoodsAddressDTO == null){
            throw new ServiceException(ResultCode.STORE_DELIVER_ADDRESS_EXIST);
        }
        try {
            Order order = orderDetailVO.getOrder();
            Map<String, Object> msgDataMap = new HashMap<String, Object>();
            msgDataMap.put("language", "zh-CN");
            msgDataMap.put("orderId", order.getSn());
            //托寄物信息
            List<Map<String, Object>> cargoDetails = new ArrayList<>();
            for (OrderItem orderItem : orderDetailVO.getOrderItems()) {
                Map<String, Object> map = new HashMap<>();
                map.put("name", orderItem.getGoodsName());
                cargoDetails.add(map);
            }
            msgDataMap.put("cargoDetails", cargoDetails);

            //收寄双方信息
            List<Map<String, Object>> contactInfoList = new ArrayList<>();
            Map<String, Object> storeContactInfoMap = new HashMap<>();
            storeContactInfoMap.put("contactType", 1);
            storeContactInfoMap.put("contact", storeDeliverGoodsAddressDTO.getSalesConsignorName());
            storeContactInfoMap.put("mobile", storeDeliverGoodsAddressDTO.getSalesConsignorMobile());
            //国家或地区2位代码 参照附录
            storeContactInfoMap.put("country", "CN");
            //详细地址，若有四级行政区划，如镇/街道等信息可拼接至此字段，格式样例：镇/街道+详细地址。若province/city 字段的值不传，此字段必须包含省市信息，避免影响原寄地代码识别，如：广东省深圳市福田区新洲十一街万基商务大厦10楼；此字段地址必须详细，否则会影响目的地中转识别；
            storeContactInfoMap.put("address", storeDeliverGoodsAddressDTO.getSalesConsignorAddressPath() + storeDeliverGoodsAddressDTO.getSalesConsignorDetail());
            contactInfoList.add(storeContactInfoMap);

            Map<String, Object> memberContactInfoMap = new HashMap<>();
            memberContactInfoMap.put("contactType", 2);
            memberContactInfoMap.put("contact", order.getConsigneeName());
            memberContactInfoMap.put("mobile", order.getConsigneeMobile());
            //国家或地区2位代码 参照附录
            memberContactInfoMap.put("country", "CN");
            //详细地址，若有四级行政区划，如镇/街道等信息可拼接至此字段，格式样例：镇/街道+详细地址。若province/city 字段的值不传，此字段必须包含省市信息，避免影响原寄地代码识别，如：广东省深圳市福田区新洲十一街万基商务大厦10楼；此字段地址必须详细，否则会影响目的地中转识别；
            memberContactInfoMap.put("address", order.getConsigneeAddressPath() + order.getConsigneeDetail());
            contactInfoList.add(memberContactInfoMap);
            msgDataMap.put("contactInfoList", contactInfoList);

            msgDataMap.put("expressTypeId", 1);
            msgDataMap.put("isReturnRoutelabel", 1);


            String result = sendPost(ExpressServiceCodeEnum.EXP_RECE_CREATE_ORDER, msgDataMap);
            JSONObject resultData = JSONUtil.parseObj(result).getJSONObject("apiResultData");
            if(Boolean.TRUE.toString().equals(resultData.get("success").toString())){
                return resultData.getJSONObject("msgData").getJSONArray("waybillNoInfoList").getJSONObject(0).get("waybillNo").toString();
            }
            throw new ServiceException(resultData.get("errorMsg").toString());
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * 文档地址：https://open.sf-express.com/Api/ApiDetails?apiServiceCode=EXP_RECE_SEARCH_ROUTES&category=1&apiClassify=1&interName=%E8%B7%AF%E7%94%B1%E6%9F%A5%E8%AF%A2%E6%8E%A5%E5%8F%A3-EXP_RECE_SEARCH_ROUTES
     *
     * @param logistics 物流公司
     * @param expNo
     * @param phone
     * @return
     */
    @Override
    public Traces pollQuery(Logistics logistics, String expNo, String phone) {
        try {
            Map<String, Object> msgDataMap = new HashMap<String, Object>();
            msgDataMap.put("language", "zh-CN");
            /**
             * 查询号类别:
             * 1:根据顺丰运单号查询,trackingNumber将被当作顺丰运单号处理
             * 2:根据客户订单号查询,trackingNumber将被当作客户订单号处理
             */
            msgDataMap.put("trackingType", 1);
            List<String> trackingNumber = new ArrayList<>();
            trackingNumber.add(expNo);
            msgDataMap.put("trackingNumber", trackingNumber);
            JSONObject result = JSONUtil.parseObj(sendPost(ExpressServiceCodeEnum.EXP_RECE_SEARCH_ROUTES, msgDataMap));
            JSONObject resultData = result.getJSONObject("apiResultData");
            if(Boolean.TRUE.toString().equals(resultData.get("success").toString())){
                JSONArray routesJson = resultData.getJSONObject("msgData").getJSONArray("routeResps").getJSONObject(0).getJSONArray("routes");
                List<Map> routes = routesJson.toList(Map.class);
                return new Traces(logistics.getName(),expNo,routes);
            }
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException(e);
        }
        return null;
    }

    @Override
    public Traces pollMapTrack(Logistics logistics, String expNo, String phone, String from, String to) {
        return null;
    }

    /**
     * 文档地址：http://open.sf-express.com/Api/ApiDetails?level3=317&interName=%E4%BA%91%E6%89%93%E5%8D%B0%E9%9D%A2%E5%8D%952.0%E6%8E%A5%E5%8F%A3-COM_RECE_CLOUD_PRINT_WAYBILLS
     *
     * @param labelOrderDTO 电子面单DTO
     * @return
     */
    @Override
    public Map<String,Object> labelOrder(LabelOrderDTO labelOrderDTO) {
        try {
            Map<String, Object> msgDataMap = new HashMap<>();
            //模板编码
            //关联云打印接口后，点击查看，可在接口详情页获取模板编码，类似：fm_76130_standard_{partnerId}
            msgDataMap.put("templateCode", logisticsSetting.getTemplateCode());
            //业务数据
            Map<String, Object> documents = new HashMap<>();
            documents.put("masterWaybillNo", labelOrderDTO.getOrder().getLogisticsNo());
            msgDataMap.put("documents",documents);
            msgDataMap.put("sync",true);
            /**
             * 版本号，传固定值:2.0
             */
            msgDataMap.put("version", "2.0");
            JSONObject result = JSONUtil.parseObj(sendPost(ExpressServiceCodeEnum.COM_RECE_CLOUD_PRINT_WAYBILLS, msgDataMap));
            JSONObject resultData = result.getJSONObject("apiResultData");
            if(Boolean.TRUE.toString().equals(resultData.get("success").toString())){
                return resultData.getJSONObject("obj").getJSONArray("files").toList(Map.class).get(0);
            }
            throw new ServiceException(resultData.getJSONArray("errorMessage").get(0).toString());
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * 文档地址：https://open.sf-express.com/Api/ApiDetails?level3=409&interName=%E9%A2%84%E8%AE%A1%E6%B4%BE%E9%80%81%E6%97%B6%E9%97%B4%E6%8E%A5%E5%8F%A3-EXP_RECE_SEARCH_PROMITM
     *
     * @param searchNo
     * @param checkNos
     */
    public String searchPromitm(String searchNo, String checkNos) {
        try {
            Map<String, Object> msgDataMap = new HashMap<String, Object>();
            //顺丰运单号
            msgDataMap.put("searchNo", searchNo);
            //校验类型 1，电话号码校验 2，月结卡号校验
            msgDataMap.put("checkType", 1);
            //校验值 当校验类型为1时传电话号码 当校验类型为2时传月结卡号
            List<String> mobileList= new ArrayList<>();
            mobileList.add(checkNos);
            msgDataMap.put("checkNos", mobileList);
            JSONObject result = JSONUtil.parseObj(sendPost(ExpressServiceCodeEnum.EXP_RECE_SEARCH_PROMITM, msgDataMap));
            JSONObject resultData = result.getJSONObject("apiResultData");
            if(Boolean.TRUE.toString().equals(resultData.get("success").toString())){
                return  resultData.getJSONObject("msgData").get("promiseTm").toString();
            }
            throw new ServiceException(resultData.get("errorMsg").toString());
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException(e);
        }
    }

    private String sendPost(IServiceCodeStandard standardService, Map<String, Object> msgDataMap) throws UnsupportedEncodingException {
        CallExpressServiceTools tools = CallExpressServiceTools.getInstance();
        Map<String, String> params = new HashMap<String, String>();
        String timeStamp = String.valueOf(System.currentTimeMillis());
        // 顾客编码
        params.put("partnerID", logisticsSetting.getClientCode());
        params.put("requestID", UUID.randomUUID().toString().replace("-", ""));
        // 接口服务码
        params.put("serviceCode", standardService.getCode());
        params.put("timestamp", timeStamp);
        params.put("msgData", JSONUtil.toJsonStr(msgDataMap));

        params.put("msgDigest", tools.getMsgDigest(params.get("msgData"), timeStamp, logisticsSetting.getCheckWord()));
        String result = HttpClientUtil.post(logisticsSetting.getCallUrl(), params);

        log.info("===调用地址 ===" + logisticsSetting.getCallUrl());
        log.info("===顾客编码 ===" + logisticsSetting.getClientCode());
        log.info("===返回结果：" + result);

        return result;
    }
}
