package cn.lili.modules.logistics.plugin.kdniao;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.logistics.LogisticsPlugin;
import cn.lili.modules.logistics.entity.dto.LabelOrderDTO;
import cn.lili.modules.logistics.entity.enums.LogisticsEnum;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.vo.OrderDetailVO;
import cn.lili.modules.store.entity.dos.StoreLogistics;
import cn.lili.modules.store.entity.dto.StoreDeliverGoodsAddressDTO;
import cn.lili.modules.system.entity.dos.Logistics;
import cn.lili.modules.system.entity.dto.LogisticsSetting;
import cn.lili.modules.system.entity.vo.Traces;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;

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
 * 快递鸟插件
 *
 * @author Bulbasaur
 */
@Slf4j
public class KdniaoPlugin implements LogisticsPlugin {

    @Autowired
    private LogisticsSetting logisticsSetting;

    public KdniaoPlugin(LogisticsSetting logisticsSetting) {
        this.logisticsSetting = logisticsSetting;
    }

    @Override
    public LogisticsEnum pluginName() {
        return LogisticsEnum.KDNIAO;
    }

    @Override
    public Traces pollQuery(Logistics logistics, String expNo, String phone) {
        try {
            String requestData = "{'OrderCode':'','ShipperCode':'" + logistics.getCode() +
                    "','LogisticCode':'" + expNo + "'" +
                    ",'CustomerName':'" + phone.substring(phone.length() - 4) + "'" +
                    "}";
            //请求地址-测试地址
            String testReqURL = "http://sandboxapi.kdniao.com:8080/kdniaosandbox/gateway/exterfaceInvoke.json";
            //请求地址-正式地址
            String reqURL = "https://api.kdniao.com/Ebusiness/EbusinessOrderHandle.aspx";
            Map<String, String> params = new HashMap<>(8);
            params.put("RequestData", urlEncoder(requestData, "UTF-8"));
            params.put("EBusinessID", logisticsSetting.getKdniaoEbusinessID());
            params.put("RequestType", "1002");
            String dataSign = encrypt(requestData, logisticsSetting.getKdniaoAppKey(), "UTF-8");
            params.put("DataSign", urlEncoder(dataSign, "UTF-8"));
            params.put("DataType", "2");

            String result = sendPost(reqURL, params);
            Map map = (Map) JSON.parse(result);
            return new Traces(logistics.getName(), expNo, (List<Map>) map.get("Traces"));
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    @Override
    public Traces pollMapTrack(Logistics logistics, String expNo, String phone, String from, String to) {
        try {
            //请求地址-测试地址
            String testReqURL = "http://sandboxapi.kdniao.com:8080/kdniaosandbox/gateway/exterfaceInvoke.json";
            //请求地址-正式地址
            String reqURL = "https://api.kdniao.com/Ebusiness/EbusinessOrderHandle.aspx";
            String RequestData = "{" +
                    "'OrderCode': ''," +
                    "'CustomerName': '" + phone.substring(phone.length() - 4) + "'," +
                    "'ShipperCode': '" + logistics.getCode() + "'," +
                    "'LogisticCode': '" + expNo + "'," +
                    "'SenderCityName': '" + from + "'," +
                    "'ReceiverCityName': '" + to + "'," +
                    "'IsReturnCoordinates': 1," +
                    "'IsReturnRouteMap': 1," +
                    "}";
            // 组装系统级参数
            Map<String, String> params = new HashMap<String, String>();
            params.put("RequestData", urlEncoder(RequestData, "UTF-8"));
            params.put("EBusinessID", logisticsSetting.getKdniaoEbusinessID());
            params.put("RequestType", "8003");
            String dataSign = encrypt(RequestData, logisticsSetting.getKdniaoAppKey(), "UTF-8");
            params.put("DataSign", urlEncoder(dataSign, "UTF-8"));
            // params.put("DataType", "2");
            // 以form表单形式提交post请求，post请求体中包含了应用级参数和系统级参数
            String result = sendPost(reqURL, params);
            log.error(result);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    @Override
    public Map<String,Object> labelOrder(LabelOrderDTO labelOrderDTO) {
        try {
            Map<String,Object> resultMap = new HashMap();
            //订单
            Order order = labelOrderDTO.getOrder();
            //订单货物
            List<OrderItem> orderItems = labelOrderDTO.getOrderItems();
            //获取对应物流
            Logistics logistics = labelOrderDTO.getLogistics();
            //收件人地址
            String[] ConsigneeAddress = order.getConsigneeAddressPath().split(",");
            //获取店家信息
            StoreDeliverGoodsAddressDTO storeDeliverGoodsAddressDTO = labelOrderDTO.getStoreDeliverGoodsAddressDTO();
            //发件人地址
            String[] consignorAddress = storeDeliverGoodsAddressDTO.getSalesConsignorAddressPath().split(",");
            //店铺-物流公司设置
            StoreLogistics storeLogistics = labelOrderDTO.getStoreLogistics();

            //组装快递鸟应用级参数
            String resultDate = "{" +
                    "'OrderCode': '" + order.getSn() + "'," + //订单编码
                    "'ShipperCode': '" + logistics.getCode() + "'," +   //快递公司编码
                    "'CustomerName': '" + storeLogistics.getCustomerName() + "'," +//客户编码
                    "'CustomerPwd': '" + storeLogistics.getCustomerPwd() + "'," +     //客户密码
                    "'MonthCode': '" + storeLogistics.getMonthCode() + "'," +       //密钥
                    "'SendSite': '" + storeLogistics.getSendSite() + "'," +         //归属网点
                    "'SendStaff': '" + storeLogistics.getSendStaff() + "'," +       //收件快递员
                    "'PayType': " + storeLogistics.getPayType() + "," +
                    "'ExpType': " + storeLogistics.getExpType() + "," +
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
                    "'Quantity': " + orderItems.size() + "," +  //包裹数
                    "'IsReturnPrintTemplate':1," +  //生成电子面单模板
                    "'Remark': '" + order.getRemark() + "'" +//商家备注
                    "}";


            //组织系统级参数
            Map<String, String> params = new HashMap<>();
            //请求地址-测试地址
            String testReqURL = "http://sandboxapi.kdniao.com:8080/kdniaosandbox/gateway/exterfaceInvoke.json";
            //请求地址-正式地址
            String reqURL = "https://api.kdniao.com/api/EOrderService";

            //进行格式加密
            params.put("RequestData", urlEncoder(resultDate, "UTF-8"));
            params.put("EBusinessID", logisticsSetting.getKdniaoEbusinessID());
            params.put("RequestType", "1007");

            String dataSign = encrypt(resultDate, logisticsSetting.getKdniaoAppKey(), "UTF-8");
            params.put("DataSign", dataSign);
            params.put("DataType", "2");
            // 以form表单形式提交post请求，post请求体中包含了应用级参数和系统级参数
            String result = sendPost(reqURL, params);
            if (CharSequenceUtil.isEmpty(result) || CharSequenceUtil.isBlank(result)) {
                throw new ServiceException(ResultCode.LOGISTICS_CHECK_SETTING);
            }
            //根据公司业务处理返回的信息......
            JSONObject obj = JSONObject.parseObject(result);
            log.info("电子面单响应：{}", result);
            if (!"100".equals(obj.getString("ResultCode"))) {
                resultMap.put("Reason",obj.getString("Reason"));
                return resultMap;
            }

            JSONObject orderJson = JSONObject.parseObject(obj.getString("Order"));
            resultMap.put("printTemplate",obj.getString("PrintTemplate"));
            return resultMap;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    @Override
    public String createOrder(OrderDetailVO orderDetailVO) {
        return null;
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
            log.error("向指定 URL 发送POST方法的请求错误", e);
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
