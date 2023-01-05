package cn.lili.modules.kdBrid.serviceImpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.modules.kdBrid.service.KdNiaoService;
import cn.lili.modules.member.service.StoreLogisticsService;
import cn.lili.modules.order.order.aop.OrderLogPoint;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.enums.DeliverStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.store.entity.dos.StoreLogistics;
import cn.lili.modules.store.entity.dto.StoreDeliverGoodsAddressDTO;
import cn.lili.modules.store.service.StoreDetailService;
import cn.lili.modules.system.entity.dos.Logistics;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.KuaidiSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.LogisticsService;
import cn.lili.modules.system.service.SettingService;
import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.google.gson.Gson;
import groovy.util.logging.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.io.*;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 快递鸟电子面单业务层实现
 *
 * @author chc
 * @since 2022-4-12 10:12:43
 */
@Service
@Slf4j
public class KdNiaoServiceImpl implements KdNiaoService {
    /**
     * 订单货物
     */
    @Autowired
    OrderItemService orderItemService;

    /**
     * 订单
     */
    @Autowired
    OrderService orderService;

    /**
     * 物流公司
     */
    @Autowired
    LogisticsService logisticsService;

    /**
     * 商家店铺
     */
    @Autowired
    StoreDetailService storeDetailService;

    /**
     * 配置
     */
    @Autowired
    SettingService settingService;

    /**
     * 店铺-物流
     */
    @Autowired
    StoreLogisticsService storeLogisticsService;


    @Override
    @OrderLogPoint(description = "'订单['+#orderSn+']发货，发货单号['+#logisticsNo+'],已打印电子面单'", orderSn = "#orderSn")
    @Transactional(rollbackFor = Exception.class)
    public String createElectronicsFaceSheet(String orderSn, String logisticsId) throws Exception {
        //电子面单模板
        String printTemplate = null;
        //获取订单及子订单
        Order order = OperationalJudgment.judgment(orderService.getBySn(orderSn));
        List<OrderItem> orderItems = orderItemService.getByOrderSn(orderSn);

        Setting setting = settingService.get(SettingEnum.KUAIDI_SETTING.name());
        if (CharSequenceUtil.isBlank(setting.getSettingValue())) {
            throw new ServiceException(ResultCode.LOGISTICS_NOT_SETTING);
        }
        KuaidiSetting kuaidiSetting = new Gson().fromJson(setting.getSettingValue(), KuaidiSetting.class);

        //ID
        String EBusinessID = kuaidiSetting.getEbusinessID();

        //KEY
        String AppKey = kuaidiSetting.getAppKey();

        //请求url
        String ReqURL = kuaidiSetting.getSheetReqURL();

        //如果订单未发货，并且订单状态值等于待发货
        if (order.getDeliverStatus().equals(DeliverStatusEnum.UNDELIVERED.name()) && order.getOrderStatus().equals(OrderStatusEnum.UNDELIVERED.name())) {

            //获取对应物流
            Logistics logistics = logisticsService.getById(logisticsId);

            //物流为空,抛出异常
            if (logistics == null) {
                throw new ServiceException(ResultCode.ORDER_LOGISTICS_ERROR);
            }

            //获取店家的物流信息
            LambdaQueryWrapper<StoreLogistics> lambdaQueryWrapper = Wrappers.lambdaQuery();
            lambdaQueryWrapper.eq(StoreLogistics::getLogisticsId, logisticsId);
            lambdaQueryWrapper.eq(StoreLogistics::getStoreId, order.getStoreId());
            StoreLogistics storeLogistics = storeLogisticsService.getOne(lambdaQueryWrapper);

            //获取店家信息
            StoreDeliverGoodsAddressDTO storeDeliverGoodsAddressDTO = storeDetailService.getStoreDeliverGoodsAddressDto(order.getStoreId());
            //收件人地址
            String[] ConsigneeAddress = order.getConsigneeAddressPath().split(",");
            //发件人地址
            String[] consignorAddress = storeDeliverGoodsAddressDTO.getSalesConsignorAddressPath().split(",");

            //组装快递鸟应用级参数
            String resultDate = "{" +
                    "'OrderCode': '" + orderSn + "'," + //订单编码
                    "'ShipperCode': '" + logistics.getCode() + "'," +   //快递公司编码
                    "'CustomerName': '"+storeLogistics.getCustomerName()+"'," +//客户编码
                    "'CustomerPwd': '"+storeLogistics.getCustomerPwd()+"'," +     //客户密码
                    "'MonthCode': '"+storeLogistics.getMonthCode()+"'," +       //密钥
                    "'SendSite': '"+storeLogistics.getSendSite()+"'," +         //归属网点
                    "'SendStaff': '"+storeLogistics.getSendStaff()+"'," +       //收件快递员
                    "'PayType': "+storeLogistics.getPayType()+"," +
                    "'ExpType': "+storeLogistics.getExpType()+"," +
                    //发件人信息
                    "'Sender': {" +
                    "'Name': '" + storeDeliverGoodsAddressDTO.getSalesConsignorName() + "'," +
                    "'Mobile': '" + storeDeliverGoodsAddressDTO.getSalesConsignorMobile() + "'," +
                    "'ProvinceName': '" + consignorAddress[0] + "'," +   //省
                    "'CityName': '" + consignorAddress[1] + "'," +       //市
                    "'ExpAreaName': '" + consignorAddress[2] + "'," +    //区
                    "'Address': '" + storeDeliverGoodsAddressDTO.getSalesConsignorDetail() + "'" +   //发件人详细地址
                    "}," +
                    //收件人信息
                    "'Receiver': {" +
                    "'Name': '" + order.getConsigneeName() + "'," +
                    "'Mobile': '" + order.getConsigneeMobile() + "'," +
                    "'ProvinceName': '" + ConsigneeAddress[0] + "'," +     //省
                    "'CityName': '" + ConsigneeAddress[1] + "'," +       //市
                    "'ExpAreaName': '" + ConsigneeAddress[2] + "'," +    //区
                    "'Address': '" + order.getConsigneeDetail() + "'" +  //收件人详细地址
                    "}," +
                    //商品信息
                    "'Commodity': [";

            //子订单信息
            for (OrderItem orderItem : orderItems) {
                resultDate = resultDate + "{" +
                        "'GoodsName': '" + orderItem.getGoodsName() + "'," +
                        "'Goodsquantity': '" + orderItem.getNum() + "'" +
                        "},";
            }
            resultDate = resultDate + "]," +
                    "'Quantity': "+orderItems.size()+"," +  //包裹数
                    "'IsReturnPrintTemplate':1,"+  //生成电子面单模板
                    "'Remark': '" + order.getRemark() + "'"+//商家备注
                    "}";


            //组织系统级参数
            Map<String, String> params = new HashMap<>();
            //进行格式加密
            params.put("RequestData", urlEncoder(resultDate, "UTF-8"));
            params.put("EBusinessID", EBusinessID);
            params.put("RequestType", "1007");

            String dataSign = encrypt(resultDate, AppKey, "UTF-8");
            params.put("DataSign", dataSign);
            params.put("DataType", "2");
            // 以form表单形式提交post请求，post请求体中包含了应用级参数和系统级参数
            String result = sendPost(ReqURL, params);

            //根据公司业务处理返回的信息......
            JSONObject obj = JSONObject.parseObject(result);

            if(!"100".equals(obj.getString("ResultCode"))){
                return obj.getString("Reason");
            }

            JSONObject orderJson = JSONObject.parseObject(obj.getString("Order"));

            //电子面单模板
            printTemplate = obj.getString("PrintTemplate");

            //进行发货
            orderService.delivery(orderSn, orderJson.getString("LogisticCode"), logisticsId);
        }
        return printTemplate;
    }

    /**
     * MD5加密
     *
     * @param str     内容
     * @param charset 编码方式
     * @throws Exception
     */
    @SuppressWarnings("unused")
    private String MD5(String str, String charset) throws Exception {
        MessageDigest md = MessageDigest.getInstance("MD5");
        md.update(str.getBytes(charset));
        byte[] result = md.digest();
        StringBuffer sb = new StringBuffer(32);
        for (int i = 0; i < result.length; i++) {
            int val = result[i] & 0xff;
            if (val <= 0xf) {
                sb.append("0");
            }
            sb.append(Integer.toHexString(val));
        }
        return sb.toString().toLowerCase();
    }

    /**
     * base64编码
     *
     * @param str     内容
     * @param charset 编码方式di
     * @throws UnsupportedEncodingException
     */
    private String base64(String str, String charset) throws UnsupportedEncodingException {
        return base64Encode(str.getBytes(charset));
    }

    @SuppressWarnings("unused")
    private String urlEncoder(String str, String charset) throws UnsupportedEncodingException {
        return URLEncoder.encode(str, charset);
    }

    /**
     * 电商Sign签名生成
     *
     * @param content  内容
     * @param keyValue Appkey
     * @param charset  编码方式
     * @return DataSign签名
     * @throws UnsupportedEncodingException ,Exception
     */
    @SuppressWarnings("unused")
    private String encrypt(String content, String keyValue, String charset) throws Exception {
        if (keyValue != null) {
            return base64(MD5(content + keyValue, charset), charset);
        }
        return base64(MD5(content, charset), charset);
    }

    /**
     * 向指定 URL 发送POST方法的请求
     *
     * @param url    发送请求的 URL
     * @param params 请求的参数集合
     * @return 远程资源的响应结果
     */
    @SuppressWarnings("unused")
    private String sendPost(String url, Map<String, String> params) {
        OutputStreamWriter out = null;
        BufferedReader in = null;
        StringBuilder result = new StringBuilder();
        try {
            URL realUrl = new URL(url);
            HttpURLConnection conn = (HttpURLConnection) realUrl.openConnection();
            //发送POST请求必须设置如下两行
            conn.setDoOutput(true);
            conn.setDoInput(true);
            //POST方法
            conn.setRequestMethod("POST");
            //设置通用的请求属性
            conn.setRequestProperty("accept", "*/*");
            conn.setRequestProperty("connection", "Keep-Alive");
            conn.setRequestProperty("user-agent",
                    "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1;SV1)");
            conn.setRequestProperty("Content-Type", "application/x-www-form-urlencoded");
            conn.connect();
            //获取URLConnection对象对应的输出流
            out = new OutputStreamWriter(conn.getOutputStream(), StandardCharsets.UTF_8);
            //发送请求参数
            if (params != null) {
                StringBuilder param = new StringBuilder();
                for (Map.Entry<String, String> entry : params.entrySet()) {
                    if (param.length() > 0) {
                        param.append("&");
                    }
                    param.append(entry.getKey());
                    param.append("=");
                    param.append(entry.getValue());
                }
                out.write(param.toString());
            }
            //flush输出流的缓冲
            out.flush();
            //定义BufferedReader输入流来读取URL的响应
            in = new BufferedReader(
                    new InputStreamReader(conn.getInputStream(), StandardCharsets.UTF_8));
            String line;
            while ((line = in.readLine()) != null) {
                result.append(line);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        //使用finally块来关闭输出流、输入流
        finally {
            try {
                if (out != null) {
                    out.close();
                }
                if (in != null) {
                    in.close();
                }
            } catch (IOException ex) {
                ex.printStackTrace();
            }
        }
        return result.toString();
    }


    private static final char[] BASE64_ENCODE_CHARS = new char[]{
            'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
            'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
            'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
            'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
            'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
            'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
            'w', 'x', 'y', 'z', '0', '1', '2', '3',
            '4', '5', '6', '7', '8', '9', '+', '/'};

    public static String base64Encode(byte[] data) {
        StringBuffer sb = new StringBuffer();
        int len = data.length;
        int i = 0;
        int b1, b2, b3;
        while (i < len) {
            b1 = data[i++] & 0xff;
            if (i == len) {
                sb.append(BASE64_ENCODE_CHARS[b1 >>> 2]);
                sb.append(BASE64_ENCODE_CHARS[(b1 & 0x3) << 4]);
                sb.append("==");
                break;
            }
            b2 = data[i++] & 0xff;
            if (i == len) {
                sb.append(BASE64_ENCODE_CHARS[b1 >>> 2]);
                sb.append(BASE64_ENCODE_CHARS[((b1 & 0x03) << 4) | ((b2 & 0xf0) >>> 4)]);
                sb.append(BASE64_ENCODE_CHARS[(b2 & 0x0f) << 2]);
                sb.append("=");
                break;
            }
            b3 = data[i++] & 0xff;
            sb.append(BASE64_ENCODE_CHARS[b1 >>> 2]);
            sb.append(BASE64_ENCODE_CHARS[((b1 & 0x03) << 4) | ((b2 & 0xf0) >>> 4)]);
            sb.append(BASE64_ENCODE_CHARS[((b2 & 0x0f) << 2) | ((b3 & 0xc0) >>> 6)]);
            sb.append(BASE64_ENCODE_CHARS[b3 & 0x3f]);
        }
        return sb.toString();
    }

}
