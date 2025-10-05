/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

///Shortcut to insert the given binary operation into `graph` and hook it up. Returns the created node
#[macro_export]
macro_rules! hook_barith {
     ($builder:expr, $op:tt, $span:expr, [$($port:expr),*]) => {
         {
             let (node, _) = $builder
                 .connect_node(
                     OptNode::new(BinaryArith::new(BinaryArithOp::$op), $span),
                     [$($port),*],
                 )
                 .unwrap();
             node
         }
     };
 }

#[macro_export]
macro_rules! hook_uarith {
      ($builder:expr, $op:tt, $span:expr, $($port:expr)?) => {
          {
              let (node, _) = $builder
                  .connect_node(
                      OptNode::new(UnaryArith::new(UnaryArithOp::$op), $span),
                      [$($port)?],
                  )
                  .unwrap();
              node
          }
      };
  }
#[macro_export]
macro_rules! hook_buildin {
       ($builder:expr, $op:tt, $span:expr, [$($port:expr),*]) => {
           {
               let (node, _) = $builder
                   .connect_node(
                       OptNode::new(Buildin::new(BuildinOp::$op), $span),
                       [$($port),*],
                   )
                   .unwrap();
               node
           }
       };
   }
